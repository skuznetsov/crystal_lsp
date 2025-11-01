module CrystalV2
  module Compiler
    module Frontend
      struct Span
        getter start_offset : Int32
        getter end_offset : Int32
        getter start_line : Int32
        getter start_column : Int32
        getter end_line : Int32
        getter end_column : Int32

        def initialize(
          @start_offset : Int32,
          @end_offset : Int32,
          @start_line : Int32,
          @start_column : Int32,
          @end_line : Int32,
          @end_column : Int32
        )
        end

        def cover(other : Span) : Span
          start_span = start_offset <= other.start_offset ? self : other
          end_span = end_offset >= other.end_offset ? self : other
          Span.new(
            start_span.start_offset,
            end_span.end_offset,
            start_span.start_line,
            start_span.start_column,
            end_span.end_line,
            end_span.end_column
          )
        end

        def self.cover_all(spans : Enumerable(Span)) : Span
          values = spans.to_a
          raise ArgumentError.new("Span.cover_all requires at least one span") if values.empty?
          values.reduce { |acc, span| acc.cover(span) }
        end
      end
    end
  end
end
