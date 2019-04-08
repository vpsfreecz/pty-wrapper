{ mkDerivation, aeson, async, base, bytestring, data-default
, network, posix-pty, stdenv, stm, text
}:
mkDerivation {
  pname = "pty-wrapper";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring data-default network posix-pty stm text
  ];
  executableHaskellDepends = [ base bytestring ];
  homepage = "https://github.com/vpsfreecz/pty-wrapper";
  description = "PTY wrapper";
  license = stdenv.lib.licenses.bsd3;
}
