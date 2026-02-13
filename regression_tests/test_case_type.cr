abstract class Stmt
# EXPECT: done
  abstract def name : String
end
class AssignS < Stmt
  def name : String; "assign"; end
end
class ReturnS < Stmt
  def name : String; "return"; end
end
class ExprS < Stmt
  def name : String; "expr"; end
end

def classify(s : Stmt) : String
  case s
  when AssignS
    "it's assign"
  when ReturnS
    "it's return"
  when ExprS
    "it's expr"
  else
    "unknown"
  end
end

a = AssignS.new
r = ReturnS.new
e = ExprS.new
puts classify(a)
puts classify(r)
puts classify(e)
puts "done"
