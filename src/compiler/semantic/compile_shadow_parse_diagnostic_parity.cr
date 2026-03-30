require "../frontend/parser/diagnostic"

module CrystalV2
  module Compiler
    module Semantic
      class CompileShadowParseDiagnosticParity
        record Signature,
          file_path : String?,
          message : String,
          start_line : Int32,
          start_column : Int32,
          end_line : Int32,
          end_column : Int32 do
          def self.from(diagnostic : Frontend::Diagnostic) : self
            span = diagnostic.span
            new(
              diagnostic.file_path,
              diagnostic.message,
              span.start_line,
              span.start_column,
              span.end_line,
              span.end_column,
            )
          end

          def label : String
            range = "#{start_line}:#{start_column}-#{end_line}:#{end_column}"
            prefix = file_path ? "#{file_path}:#{range}" : range
            "#{prefix} #{message}"
          end
        end

        getter compile_counts : Hash(Signature, Int32)
        getter shadow_counts : Hash(Signature, Int32)

        def initialize(
          @compile_counts : Hash(Signature, Int32),
          @shadow_counts : Hash(Signature, Int32),
        )
        end

        def self.compare(
          compile_diagnostics : Array(Frontend::Diagnostic),
          shadow_diagnostics : Array(Frontend::Diagnostic)
        ) : self
          new(
            count_signatures(compile_diagnostics),
            count_signatures(shadow_diagnostics),
          )
        end

        def compile_total : Int32
          @compile_counts.values.sum
        end

        def compile_unique_count : Int32
          @compile_counts.size
        end

        def shadow_total : Int32
          @shadow_counts.values.sum
        end

        def shadow_unique_count : Int32
          @shadow_counts.size
        end

        def missing_in_shadow : Array(String)
          diff_labels(@compile_counts, @shadow_counts)
        end

        def extra_in_shadow : Array(String)
          diff_labels(@shadow_counts, @compile_counts)
        end

        def gap_count : Int32
          missing_in_shadow.size + extra_in_shadow.size
        end

        def summary_lines(max_entries : Int32 = 5, left_label : String = "compile", right_label : String = "shadow") : Array(String)
          lines = [] of String
          lines << String.build do |io|
            io << left_label << "_total=" << compile_total
            io << " " << left_label << "_unique=" << compile_unique_count
            io << " " << right_label << "_total=" << shadow_total
            io << " " << right_label << "_unique=" << shadow_unique_count
            io << " gaps=" << gap_count
          end

          missing = missing_in_shadow
          unless missing.empty?
            preview = missing.first(max_entries).join(", ")
            suffix = missing.size > max_entries ? ", ..." : ""
            lines << "  missing_in_shadow=#{preview}#{suffix}"
          end

          extra = extra_in_shadow
          unless extra.empty?
            preview = extra.first(max_entries).join(", ")
            suffix = extra.size > max_entries ? ", ..." : ""
            lines << "  extra_in_shadow=#{preview}#{suffix}"
          end

          lines
        end

        def strict_message(left_label : String = "compile", right_label : String = "shadow") : String?
          return nil if gap_count == 0

          summary = summary_lines(3, left_label, right_label)
          String.build do |io|
            io << "semantic shadow strict parse diagnostic mismatch"
            summary.each do |line|
              io << '\n' << line
            end
          end
        end

        private def self.count_signatures(diagnostics : Array(Frontend::Diagnostic)) : Hash(Signature, Int32)
          counts = {} of Signature => Int32
          diagnostics.each do |diagnostic|
            signature = Signature.from(diagnostic)
            counts[signature] = (counts[signature]? || 0) + 1
          end
          counts
        end

        private def diff_labels(
          left_counts : Hash(Signature, Int32),
          right_counts : Hash(Signature, Int32)
        ) : Array(String)
          labels = [] of String
          left_counts.each do |signature, left_count|
            delta = left_count - (right_counts[signature]? || 0)
            next unless delta > 0
            delta.times { labels << signature.label }
          end
          labels.sort
        end
      end
    end
  end
end
