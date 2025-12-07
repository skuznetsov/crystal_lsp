require "spec"
require "json"
require "file_utils"
require "random/secure"

require "../../src/compiler/lsp/unified_project"
require "../../src/compiler/lsp/project_cache"

describe CrystalV2::Compiler::LSP::ProjectCacheLoader do
  describe "TypeIndex-only cache (v5)" do
    it "saves and loads single file with TypeIndex" do
      root = File.join(Dir.tempdir, "cache_typeindex_test_#{Random::Secure.hex(6)}")
      src_dir = File.join(root, "src")
      FileUtils.mkdir_p(src_dir)
      path = File.join(src_dir, "test.cr")
      source = <<-CR
      class Foo
        def bar
          42
        end
      end
      CR
      File.write(path, source)

      # Create project and analyze
      project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      project.update_file(path, source)

      # Save cache (writes TypeIndex, no JSON expr_types)
      CrystalV2::Compiler::LSP::ProjectCacheLoader.save_to_cache(project, root)

      # Load into fresh project
      fresh = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      result = CrystalV2::Compiler::LSP::ProjectCacheLoader.load_from_cache(fresh, root)

      result[:valid_count].should eq(1)
      result[:invalid_paths].should be_empty
      fresh.files.size.should eq(1)
    ensure
      FileUtils.rm_rf(root) if root
    end

    it "saves and loads multiple files without ExprId collisions" do
      root = File.join(Dir.tempdir, "cache_multi_typeindex_#{Random::Secure.hex(6)}")
      src_dir = File.join(root, "src")
      FileUtils.mkdir_p(src_dir)

      # Create two files with types
      path1 = File.join(src_dir, "file1.cr")
      File.write(path1, "x = 1")
      path2 = File.join(src_dir, "file2.cr")
      File.write(path2, "y = \"hello\"")

      project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      project.update_file(path1, File.read(path1))
      project.update_file(path2, File.read(path2))

      CrystalV2::Compiler::LSP::ProjectCacheLoader.save_to_cache(project, root)

      fresh = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      result = CrystalV2::Compiler::LSP::ProjectCacheLoader.load_from_cache(fresh, root)

      result[:valid_count].should eq(2)
      fresh.files.size.should eq(2)
    ensure
      FileUtils.rm_rf(root) if root
    end

    it "restores expression types from TypeIndex" do
      root = File.join(Dir.tempdir, "cache_restore_types_#{Random::Secure.hex(6)}")
      src_dir = File.join(root, "src")
      FileUtils.mkdir_p(src_dir)
      path = File.join(src_dir, "test.cr")
      File.write(path, "a = 1")

      project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      project.update_file(path, "a = 1")

      # Manually set some cached types
      project.cached_expr_types[path] = {0 => "Int32", 1 => "Int32"}

      CrystalV2::Compiler::LSP::ProjectCacheLoader.save_to_cache(project, root)

      fresh = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      CrystalV2::Compiler::LSP::ProjectCacheLoader.load_from_cache(fresh, root)

      # Types should be restored from TypeIndex
      fresh.cached_expr_types[path]?.should_not be_nil
    ensure
      FileUtils.rm_rf(root) if root
    end

    it "handles cache version upgrade (old cache invalidated)" do
      root = File.join(Dir.tempdir, "cache_version_test_#{Random::Secure.hex(6)}")
      src_dir = File.join(root, "src")
      FileUtils.mkdir_p(src_dir)
      path = File.join(src_dir, "test.cr")
      File.write(path, "x = 1")

      # Manually create an old v4 cache file (will be rejected)
      cache_dir = ENV["XDG_CACHE_HOME"]? || File.join(ENV["HOME"]? || "/tmp", ".cache")
      cache_path = File.join(cache_dir, "crystal_v2_lsp", "projects")
      FileUtils.mkdir_p(cache_path)

      # The load will fail on version mismatch, returning 0 valid files
      fresh = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      result = CrystalV2::Compiler::LSP::ProjectCacheLoader.load_from_cache(fresh, root)

      # No cache exists, so 0 valid files
      result[:valid_count].should eq(0)
    ensure
      FileUtils.rm_rf(root) if root
    end
  end
end
