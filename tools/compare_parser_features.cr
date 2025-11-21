#!/usr/bin/env crystal
# Compare presence of key parsing/lexing features between original Crystal parser
# (src/compiler/crystal/syntax/*.cr) and the v2 parser (crystal_v2/src/compiler/frontend/*.cr).
# This tool searches for indicative code patterns and reports PASS/FAIL per side.

require "../src/compiler/frontend/watchdog"

struct Feature
  getter name : String
  getter desc : String
  getter file1 : String
  getter file2 : String
  getter patterns1 : Array(String)
  getter patterns2 : Array(String)

  def initialize(@name, @desc, @file1, @file2, @patterns1, @patterns2)
  end
end

def file_text(path : String) : String
  File.read(path)
rescue ex
  ""
end

def contains_all?(text : String, patterns : Array(String)) : Bool
  patterns.all? { |p| !!(text =~ Regex.new(p)) }
end

def report(feature : Feature)
  text1 = file_text(feature.file1)
  text2 = file_text(feature.file2)
  f1 = contains_all?(text1, feature.patterns1)
  f2 = contains_all?(text2, feature.patterns2)
  puts "[#{feature.name}] #{feature.desc}"
  puts "  original: #{f1 ? "PASS" : "FAIL"} (#{feature.file1})"
  puts "  v2      : #{f2 ? "PASS" : "FAIL"} (#{feature.file2})"
end

# Resolve roots dynamically so the script works inside crystal_v2_repo with a sibling crystal/ checkout.
project_root = File.expand_path("..", __DIR__)
upstream_root = ENV["CRYSTAL_UPSTREAM_ROOT"]? || File.expand_path("../crystal", project_root)
v2_root = ENV["CRYSTAL_V2_ROOT"]? || project_root

original_parser = File.join(upstream_root, "src/compiler/crystal/syntax/parser.cr")
original_lexer  = File.join(upstream_root, "src/compiler/crystal/syntax/lexer.cr")
v2_parser       = File.join(v2_root, "src/compiler/frontend/parser.cr")
v2_lexer        = File.join(v2_root, "src/compiler/frontend/lexer.cr")

features = [
  Feature.new(
    name: "case_when_then",
    desc: "case/when with optional 'then' and statement separators in branch bodies",
    file1: original_parser,
    file2: v2_parser,
    patterns1: [
      "def\\s+parse_case",
      "when_expression_end",
      "parse_expressions",
    ],
    patterns2: [
      "private\\s+def\\s+parse_case",
      "Token::Kind::Then",
      "skip_statement_end",
    ]
  ),
  Feature.new(
    name: "case_in_predicate",
    desc: "exhaustive case/in and dot-predicate (implicit obj question method)",
    file1: original_parser,
    file2: v2_parser,
    patterns1: [
      "when\\s+Keyword::IN",
      "check_valid_exhaustive_expression",
    ],
    patterns2: [
      "Token::Kind::In",
      "parse_in_pattern_expr",
    ]
  ),
  Feature.new(
    name: "no_parens_amp",
    desc: "no-parens call args support &.shorthand and &capture",
    file1: original_parser,
    file2: v2_parser,
    patterns1: [
      "call_args_without_parens",
      "&\\.",
    ],
    patterns2: [
      "parse_member_access",
      "current_token.kind == Token::Kind::Amp",
      "current_token.kind == Token::Kind::AmpDot",
      "parse_block_shorthand",
    ]
  ),
  Feature.new(
    name: "raise_no_parens_named",
    desc: "raise supports parens and no-parens with named args",
    file1: original_parser,
    file2: v2_parser,
    patterns1: [
      "def\\s+parse_return",
      "def\\s+parse_raise",
    ],
    patterns2: [
      "private\\s+def\\s+parse_raise",
      "NamedArgument",
    ]
  ),
  Feature.new(
    name: "backtick_literal",
    desc: "`command` literal tokenization with interpolation",
    file1: original_lexer,
    file2: v2_lexer,
    patterns1: [
      "`",
    ],
    patterns2: [
      "BACKTICK",
      "def\\s+lex_backtick",
      "StringInterpolation|String",
    ]
  ),
  Feature.new(
    name: "percent_regex",
    desc: "%r(...) regex literal with flags",
    file1: original_lexer,
    file2: v2_lexer,
    patterns1: [
      "%r",
    ],
    patterns2: [
      "scan_percent_literal",
      "when 'r'",
      "Token::Kind::Regex",
    ]
  ),
  Feature.new(
    name: "regex_globals",
    desc: "regex match globals: $?, $!, $1.., $~, optional '?' suffix",
    file1: original_lexer,
    file2: v2_lexer,
    patterns1: ["\\$\\?", "\\$\\!", "\\$\\~", "\\$\\d"],
    patterns2: ["lex_global_var", "QUESTION", "EXCLAMATION", "\\$", "Token::Kind::GlobalVar"],
  ),
  Feature.new(
    name: "expr_level_keywords",
    desc: "return/super/previous_def/break/next allowed in expression context",
    file1: original_parser,
    file2: v2_parser,
    patterns1: ["def\\s+parse_return", "def\\s+parse_super"],
    patterns2: ["Token::Kind::Return", "Token::Kind::Super", "Token::Kind::PreviousDef", "Token::Kind::Break", "Token::Kind::Next"],
  ),
]

puts "Compare parser/lexer feature presence (original vs v2)"
puts "- original parser: #{original_parser}"
puts "- original lexer : #{original_lexer}"
puts "- v2 parser      : #{v2_parser}"
puts "- v2 lexer       : #{v2_lexer}"
puts

features.each { |f| report(f) }
