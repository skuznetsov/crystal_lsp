require "spec"

require "../../src/main"
require "../../src/compiler/lsp/server"

describe CrystalV2::Compiler::LSP::Server do
  describe "#find_call_context" do
    it "finds call context immediately after opening paren" do
      server = CrystalV2::Compiler::LSP::Server.new
      text = "result = compute(10)"
      line = 0
      character = 17  # After '('

      result = server.find_call_context(text, line, character)
      result.should_not be_nil

      paren_pos, method_name, active_param = result.not_nil!
      paren_pos.should eq(16)
      method_name.should eq("compute")
      active_param.should eq(0)
    end

    it "calculates active parameter after first comma" do
      server = CrystalV2::Compiler::LSP::Server.new
      text = "result = compute(10, 20)"
      line = 0
      character = 21  # After ', '

      result = server.find_call_context(text, line, character)
      result.should_not be_nil

      paren_pos, method_name, active_param = result.not_nil!
      method_name.should eq("compute")
      active_param.should eq(1)
    end

    it "calculates active parameter after second comma" do
      server = CrystalV2::Compiler::LSP::Server.new
      text = "result = compute(10, 20, \"test\")"
      line = 0
      character = 29  # After second comma

      result = server.find_call_context(text, line, character)
      result.should_not be_nil

      paren_pos, method_name, active_param = result.not_nil!
      method_name.should eq("compute")
      active_param.should eq(2)
    end

    it "handles incomplete calls with trailing comma" do
      server = CrystalV2::Compiler::LSP::Server.new
      text = "result = compute(10,"
      line = 0
      character = 20  # After comma at end

      result = server.find_call_context(text, line, character)
      result.should_not be_nil

      paren_pos, method_name, active_param = result.not_nil!
      method_name.should eq("compute")
      active_param.should eq(1)
    end

    it "skips nested parentheses" do
      server = CrystalV2::Compiler::LSP::Server.new
      text = "result = outer(inner(10),"
      line = 0
      character = 25  # After comma in outer call

      result = server.find_call_context(text, line, character)
      result.should_not be_nil

      paren_pos, method_name, active_param = result.not_nil!
      method_name.should eq("outer")
      active_param.should eq(1)
    end

    it "returns nil when no opening paren found" do
      server = CrystalV2::Compiler::LSP::Server.new
      text = "result = 10 + 20"
      line = 0
      character = 10

      result = server.find_call_context(text, line, character)
      result.should be_nil
    end
  end

  describe "#extract_method_name_before" do
    it "extracts simple method name" do
      server = CrystalV2::Compiler::LSP::Server.new
      line = "result = compute(10)"
      pos = 16  # Position of '('

      name = server.extract_method_name_before(line, pos)
      name.should eq("compute")
    end

    it "extracts method name with whitespace before paren" do
      server = CrystalV2::Compiler::LSP::Server.new
      line = "result = compute  (10)"
      pos = 18  # Position of '('

      name = server.extract_method_name_before(line, pos)
      name.should eq("compute")
    end

    it "extracts method name with underscores" do
      server = CrystalV2::Compiler::LSP::Server.new
      line = "result = my_method_123(10)"
      pos = 22  # Position of '('

      name = server.extract_method_name_before(line, pos)
      name.should eq("my_method_123")
    end

    it "returns nil when no identifier before paren" do
      server = CrystalV2::Compiler::LSP::Server.new
      line = "result = (10 + 20)"
      pos = 9  # Position of '('

      name = server.extract_method_name_before(line, pos)
      name.should be_nil
    end
  end

  describe "SignatureInformation" do
    it "creates signature with parameters" do
      params = [
        CrystalV2::Compiler::LSP::ParameterInformation.new(label: "x : Int32"),
        CrystalV2::Compiler::LSP::ParameterInformation.new(label: "y : Int32")
      ]

      sig = CrystalV2::Compiler::LSP::SignatureInformation.new(
        label: "compute(x : Int32, y : Int32) : Int32",
        parameters: params
      )

      sig.label.should eq("compute(x : Int32, y : Int32) : Int32")
      sig.parameters.should_not be_nil
      params_list = sig.parameters.not_nil!
      params_list.size.should eq(2)
      params_list[0].label.should eq("x : Int32")
      params_list[1].label.should eq("y : Int32")
    end

    it "creates signature without parameters" do
      sig = CrystalV2::Compiler::LSP::SignatureInformation.new(
        label: "get_value() : Int32",
        parameters: nil
      )

      sig.label.should eq("get_value() : Int32")
      sig.parameters.should be_nil
    end
  end

  describe "SignatureHelp structure" do
    it "creates complete signature help response" do
      sigs = [
        CrystalV2::Compiler::LSP::SignatureInformation.new(
          label: "compute(x : Int32) : Int32",
          parameters: [
            CrystalV2::Compiler::LSP::ParameterInformation.new(label: "x : Int32")
          ]
        ),
        CrystalV2::Compiler::LSP::SignatureInformation.new(
          label: "compute(x : Int32, y : Int32) : Int32",
          parameters: [
            CrystalV2::Compiler::LSP::ParameterInformation.new(label: "x : Int32"),
            CrystalV2::Compiler::LSP::ParameterInformation.new(label: "y : Int32")
          ]
        )
      ]

      help = CrystalV2::Compiler::LSP::SignatureHelp.new(
        signatures: sigs,
        active_signature: 0,
        active_parameter: 1
      )

      help.signatures.size.should eq(2)
      help.active_signature.should eq(0)
      help.active_parameter.should eq(1)
    end

    it "serializes to JSON correctly" do
      sig = CrystalV2::Compiler::LSP::SignatureInformation.new(
        label: "compute(x : Int32) : Int32",
        parameters: [
          CrystalV2::Compiler::LSP::ParameterInformation.new(label: "x : Int32")
        ]
      )

      help = CrystalV2::Compiler::LSP::SignatureHelp.new(
        signatures: [sig],
        active_signature: 0,
        active_parameter: 0
      )

      json = help.to_json
      json.should contain("\"signatures\"")
      json.should contain("\"activeSignature\"")
      json.should contain("\"activeParameter\"")
      json.should contain("compute(x : Int32) : Int32")
    end
  end
end
