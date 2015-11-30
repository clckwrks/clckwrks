{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, aeson, aeson-qq, attoparsec, base
      , blaze-html, bytestring, cereal, containers, directory, filepath
      , happstack-authenticate, happstack-hsp, happstack-jmacro
      , happstack-server, happstack-server-tls, hsp, hsx-jmacro, hsx2hs
      , ixset, jmacro, lens, mtl, network, network-uri, old-locale
      , openssl, process, random, reform, reform-happstack, reform-hsp
      , safecopy, stdenv, stm, tagsoup, text, time, time-locale-compat
      , unordered-containers, userid, utf8-string, uuid-orphans
      , uuid-types, vector, web-plugins, web-routes, web-routes-happstack
      , web-routes-hsp, web-routes-th, xss-sanitize, cabal-install
      }:
      mkDerivation {
        pname = "clckwrks";
        version = "0.23.10";
        src = ./.;
        libraryHaskellDepends = [
          acid-state aeson aeson-qq attoparsec base blaze-html bytestring
          cereal containers directory filepath happstack-authenticate
          happstack-hsp happstack-jmacro happstack-server
          happstack-server-tls hsp hsx-jmacro hsx2hs ixset jmacro lens mtl
          network network-uri old-locale process random reform
          reform-happstack reform-hsp safecopy stm tagsoup text time
          time-locale-compat unordered-containers userid utf8-string
          uuid-orphans uuid-types vector web-plugins web-routes
          web-routes-happstack web-routes-hsp web-routes-th xss-sanitize
          cabal-install
        ];
        librarySystemDepends = [ openssl ];
        homepage = "http://www.clckwrks.com/";
        description = "A secure, reliable content management system (CMS) and blogging platform";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
