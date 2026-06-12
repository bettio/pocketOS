defmodule NVSFSTest do
  use ExUnit.Case

  defp fs do
    {:ok, pid} = NVSFS.start_link()
    pid
  end

  test "write/read round-trip through close" do
    pid = fs()
    blob = :crypto.strong_rand_bytes(64)

    {:ok, file} = NVSFS.open(pid, "/nodekey.bin", [:write])
    assert NVSFS.write(file, blob) == {:ok, 64}
    assert NVSFS.close(file) == :ok

    {:ok, file2} = NVSFS.open(pid, "/nodekey.bin", [:read])
    assert NVSFS.read(file2, 64) == {:ok, blob}
    assert NVSFS.close(file2) == :ok
  end

  test "reading a missing key is enoent" do
    assert NVSFS.open(fs(), "/missing.bin", [:read]) == {:error, :enoent}
  end

  test "reads past the end are short, then eof" do
    pid = fs()
    {:ok, file} = NVSFS.open(pid, "/f", [:write])
    {:ok, 5} = NVSFS.write(file, "hello")
    :ok = NVSFS.close(file)

    {:ok, file2} = NVSFS.open(pid, "/f", [:read])
    assert NVSFS.read(file2, 3) == {:ok, "hel"}
    assert NVSFS.read(file2, 100) == {:ok, "lo"}
    assert NVSFS.read(file2, 1) == :eof
  end

  test "names are capped at 15 chars, no directories" do
    pid = fs()
    assert NVSFS.open(pid, "/sixteen-chars.xx", [:write]) == {:error, :enametoolong}
    assert {:ok, file} = NVSFS.open(pid, "/fifteen.chars.x", [:write])
    assert NVSFS.close(file) == :ok
    assert NVSFS.open(pid, "/dir/file", [:write]) == {:error, :enoent}
    assert NVSFS.open(pid, "/", [:read]) == {:error, :enoent}
  end

  test "operations on a closed handle" do
    pid = fs()
    {:ok, file} = NVSFS.open(pid, "/f", [:write])
    :ok = NVSFS.close(file)
    assert NVSFS.read(file, 1) == {:error, :terminated}
    assert NVSFS.write(file, "x") == {:error, :terminated}
    assert NVSFS.close(file) == :ok
  end

  test "writing to a read-only handle is ebadf" do
    pid = fs()
    {:ok, file} = NVSFS.open(pid, "/f", [:write])
    {:ok, 1} = NVSFS.write(file, "x")
    :ok = NVSFS.close(file)

    {:ok, file2} = NVSFS.open(pid, "/f", [:read])
    assert NVSFS.write(file2, "y") == {:error, :ebadf}
  end

  test "write mode truncates, sequential writes concatenate" do
    pid = fs()
    {:ok, file} = NVSFS.open(pid, "/f", [:write])
    {:ok, 3} = NVSFS.write(file, "abc")
    {:ok, 3} = NVSFS.write(file, "def")
    :ok = NVSFS.close(file)

    {:ok, file2} = NVSFS.open(pid, "/f", [:write])
    :ok = NVSFS.close(file2)

    {:ok, file3} = NVSFS.open(pid, "/f", [:read])
    assert NVSFS.read(file3, 100) == :eof
  end

  test "read-write mode overwrites in place and keeps the tail" do
    pid = fs()
    {:ok, file} = NVSFS.open(pid, "/f", [:write])
    {:ok, 6} = NVSFS.write(file, "abcdef")
    :ok = NVSFS.close(file)

    {:ok, file2} = NVSFS.open(pid, "/f", [:read, :write])
    {:ok, 2} = NVSFS.write(file2, "XY")
    :ok = NVSFS.close(file2)

    {:ok, file3} = NVSFS.open(pid, "/f", [:read])
    assert NVSFS.read(file3, 100) == {:ok, "XYcdef"}
  end

  test "NodeKey persists and reloads through NVS0" do
    case FSRegistry.start_link() do
      {:ok, _} -> :ok
      {:error, {:already_started, _}} -> :ok
    end

    :ok = FSRegistry.register_fs("NVS0", fs())

    {pub, priv} = NodeKey.load_or_generate("NVS0:/nodekey.bin")
    assert byte_size(pub) == 32 and byte_size(priv) == 32

    # second call loads the persisted pair instead of generating a new one
    assert NodeKey.load_or_generate("NVS0:/nodekey.bin") == {pub, priv}
  end
end
