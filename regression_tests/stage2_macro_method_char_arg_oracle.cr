macro probe(*methods)
  {% for method in methods %}
    {% if method.id.ends_with?('=') %}
      1
    {% else %}
      2
    {% end %}
  {% end %}
end
