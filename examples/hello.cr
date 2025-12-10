# Simple test program for Crystal v2 compiler

def add(a : Int32, b : Int32) : Int32
  a + b
end

def main
  x = 10
  y = 20
  result = add(x, y)
  result
end
