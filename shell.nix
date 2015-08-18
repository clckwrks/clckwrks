with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, acid-state, aeson, aeson-qq, attoparsec, base
             , blaze-html, bytestring, cereal, containers, directory, filepath
             , happstack-authenticate, happstack-hsp, happstack-jmacro
             , happstack-server, happstack-server-tls, hsp, hsx-jmacro, hsx2hs
             , ixset, jmacro, lens, mtl, network, network-uri, old-locale
             , openssl, process, random, reform, reform-happstack, reform-hsp
             , safecopy, stdenv, stm, tagsoup, text, time, time-locale-compat
             , unordered-containers, utf8-string, uuid, vector, web-plugins
             , web-routes, web-routes-happstack, web-routes-hsp, web-routes-th
             , xss-sanitize, userid
             }:
             mkDerivation {
               pname = "clckwrks";
               version = "0.23.6";
               src = ./.;
               buildDepends = [
                 acid-state aeson aeson-qq attoparsec base blaze-html bytestring
                 cereal containers directory filepath happstack-authenticate
                 happstack-hsp happstack-jmacro happstack-server
                 happstack-server-tls hsp hsx-jmacro hsx2hs ixset jmacro lens mtl
                 network network-uri old-locale process random reform
                 reform-happstack reform-hsp safecopy stm tagsoup text time
                 time-locale-compat unordered-containers utf8-string uuid vector
                 web-plugins web-routes web-routes-happstack web-routes-hsp
                 web-routes-th xss-sanitize userid
               ];
               buildTools = [ hsx2hs ];
               extraLibraries = [ openssl ];
               homepage = "http://www.clckwrks.com/";
               description = "A secure, reliable content management system (CMS) and blogging platform";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
