class User
  property name : String
  property email : String

  def initialize(@name, @email)
  end

  def display
    "#{name} <#{email}>"
  end
end

def create_user(name : String, email : String) : User
  User.new(name, email)
end

user = create_user("Alice", "alice@example.com")
puts user.display
puts user.name
