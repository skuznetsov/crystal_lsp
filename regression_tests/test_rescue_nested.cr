def nested_rescue : String
  begin
    begin
      raise "inner"
    rescue ex
      "caught inner: #{ex.message}"
    end
  rescue
    "caught outer"
  end
end

puts nested_rescue
