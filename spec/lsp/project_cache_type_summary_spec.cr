require "spec"
require "file_utils"
require "random/secure"

require "../../src/compiler/lsp/unified_project"
require "../../src/compiler/lsp/project_cache"

describe CrystalV2::Compiler::LSP::UnifiedProjectState do
  it "persists inferred types into cache and rebuilds cached types on load" do
    root = File.join(Dir.tempdir, "cache_type_test_#{Random::Secure.hex(6)}")
    src_dir = File.join(root, "src")
    FileUtils.mkdir_p(src_dir)
    path = File.join(src_dir, "cache_type_test.cr")
    source = <<-CR
    module M
      def self.answer
        1
      end
    end
    CR
    File.write(path, source)

    project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
    project.update_file(path, source)

    state = project.files[path].not_nil!
    summary = state.symbol_summaries.find { |s| s.name == "M" }
    summary.should_not be_nil
    summary.not_nil!.inferred_type.should_not be_nil

    project.cached_expr_types[path].should_not be_empty

    # Save cache
    cache = CrystalV2::Compiler::LSP::ProjectCache.from_project(project, root)
    cache.save

    # Load into a fresh project and rebuild cached types
    fresh = CrystalV2::Compiler::LSP::UnifiedProjectState.new
    result = CrystalV2::Compiler::LSP::ProjectCacheLoader.load_from_cache(fresh, root)
    result[:valid_count].should eq(1)
    fresh.cached_types[path]["M"].should_not be_nil
    fresh.cached_expr_types[path].should_not be_empty
  ensure
    FileUtils.rm_rf(root) if root
  end
end
