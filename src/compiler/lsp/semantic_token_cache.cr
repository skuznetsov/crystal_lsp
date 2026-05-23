module CrystalV2
  module Compiler
    module LSP
      class SemanticTokenDiskCache
        MAGIC            = "CV2S"
        VERSION          = 3_u32
        MIN_SOURCE_BYTES = 64 * 1024

        def self.cache_path(file_path : String) : String
          cache_dir = ENV["XDG_CACHE_HOME"]? || File.join(ENV["HOME"]? || "/tmp", ".cache")
          hash = fnv_hash(file_path).to_s(16)
          File.join(cache_dir, "crystal_v2_lsp", "semantic_tokens", "v#{VERSION}", "#{hash}.json")
        end

        def self.load(file_path : String, source_mtime_ns : Int64, source_size : UInt64) : String?
          path = cache_path(file_path)
          data = File.read(path).to_slice rescue return nil
          return nil if data.size < 32

          io = IO::Memory.new(data)
          magic = Bytes.new(4)
          io.read_fully(magic)
          return nil unless String.new(magic) == MAGIC

          version = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          return nil unless version == VERSION

          cached_compiler_fingerprint = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
          return nil unless cached_compiler_fingerprint == AstCache.compiler_fingerprint

          cached_mtime_ns = io.read_bytes(Int64, IO::ByteFormat::LittleEndian)
          return nil unless cached_mtime_ns == source_mtime_ns

          cached_size = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
          return nil unless cached_size == source_size

          json_size = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          return nil unless json_size <= data.size - io.pos

          String.new(data[io.pos, json_size])
        rescue
          nil
        end

        def self.save(file_path : String, source_mtime_ns : Int64, source_size : UInt64, json : String)
          path = cache_path(file_path)
          Dir.mkdir_p(File.dirname(path))
          tmp_path = "#{path}.tmp.#{Process.pid}"

          File.open(tmp_path, "wb") do |io|
            io.write(MAGIC.to_slice)
            io.write_bytes(VERSION, IO::ByteFormat::LittleEndian)
            io.write_bytes(AstCache.compiler_fingerprint, IO::ByteFormat::LittleEndian)
            io.write_bytes(source_mtime_ns, IO::ByteFormat::LittleEndian)
            io.write_bytes(source_size, IO::ByteFormat::LittleEndian)
            io.write_bytes(json.bytesize.to_u32, IO::ByteFormat::LittleEndian)
            io.write(json.to_slice)
          end

          File.rename(tmp_path, path)
        rescue
          File.delete?(tmp_path) if tmp_path
        end

        private def self.fnv_hash(str : String) : UInt64
          hash = 14695981039346656037_u64
          str.each_byte do |byte|
            hash ^= byte.to_u64
            hash &*= 1099511628211_u64
          end
          hash
        end
      end
    end
  end
end
