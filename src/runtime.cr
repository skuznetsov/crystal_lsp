require "set"

module CrystalV2
  module Runtime
    @@target_flags : Set(String)?

    def self.bootstrap
      # Placeholder for future runtime initialization (GC tuning, fiber pools, etc.).
    end

    def self.target_flags : Set(String)
      @@target_flags ||= begin
        flags = Set(String).new

        {% if flag?(:bits64) %}
          flags << "bits64"
        {% elsif flag?(:bits32) %}
          flags << "bits32"
        {% end %}

        {% if flag?(:little_endian) %}
          flags << "little_endian"
        {% elsif flag?(:big_endian) %}
          flags << "big_endian"
        {% end %}

        {% if flag?(:x86_64) %}
          flags << "x86_64"
          flags << "x86"
        {% elsif flag?(:i386) || flag?(:x86) %}
          flags << "x86"
          flags << "i386"
        {% end %}

        {% if flag?(:arm) %}
          flags << "arm"
        {% end %}
        {% if flag?(:armhf) %}
          flags << "armhf"
        {% end %}
        {% if flag?(:aarch64) %}
          flags << "aarch64"
          flags << "arm64"
        {% end %}
        {% if flag?(:wasm32) %}
          flags << "wasm32"
        {% end %}

        {% if flag?(:linux) %}
          flags << "linux"
          flags << "unix"
        {% end %}
        {% if flag?(:darwin) %}
          flags << "darwin"
          flags << "unix"
        {% end %}
        {% if flag?(:freebsd) %}
          flags << "freebsd"
          flags << "unix"
        {% end %}
        {% if flag?(:netbsd) %}
          flags << "netbsd"
          flags << "unix"
        {% end %}
        {% if flag?(:openbsd) %}
          flags << "openbsd"
          flags << "unix"
        {% end %}
        {% if flag?(:dragonfly) %}
          flags << "dragonfly"
          flags << "unix"
        {% end %}
        {% if flag?(:win32) %}
          flags << "win32"
          flags << "windows"
        {% end %}
        {% if flag?(:windows) %}
          flags << "windows"
        {% end %}

        {% if flag?(:gnu) %}
          flags << "gnu"
        {% end %}
        {% if flag?(:musl) %}
          flags << "musl"
        {% end %}
        {% if flag?(:msvc) %}
          flags << "msvc"
        {% end %}

        unless flags.includes?("little_endian") || flags.includes?("big_endian")
          flags << "little_endian"
        end

        flags
      end
    end
  end
end
