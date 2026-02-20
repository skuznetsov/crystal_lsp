# EXPECT: enum_methods_ok
# Tests enum definitions with custom methods and value access.
# Exercises enum codegen, .value, case/when on enums.

enum Color
  Red
  Green
  Blue

  def primary? : Bool
    self == Color::Red || self == Color::Blue
  end

  def label : String
    case self
    when Color::Red   then "red"
    when Color::Green then "green"
    when Color::Blue  then "blue"
    else "unknown"
    end
  end
end

c1 = Color::Red
c2 = Color::Green
c3 = Color::Blue

labels = [c1.label, c2.label, c3.label]
primaries = [c1.primary?, c2.primary?, c3.primary?]

if labels[0] == "red" && labels[1] == "green" && labels[2] == "blue" &&
   primaries[0] == true && primaries[1] == false && primaries[2] == true
  puts "enum_methods_ok"
else
  puts "enum_methods_bad"
end
