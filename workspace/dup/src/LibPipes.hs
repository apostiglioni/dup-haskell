
--hashFile :: FilePath -> Producer (Digest MD5) IO ()
hashFileWorking file = withFile file ReadMode $ \h ->
  runEffect $
    for (PB.fromHandle h) $ \b ->
      lift $ print b

hashBytes ctx = do
  bytes <- await

--  lift $ print "Hola Mundo"
--  lift $ print bytes
--  lift $ print "Hola Mundo"

--  hashBytes (hashUpdate (ctx :: Context MD5) bytes)
--  hashBytes (hashUpdate ctx bytes)
  hashBytes (hashUpdate ctx bytes)


hashFile file = withFile file ReadMode $ \h ->
  runEffect $ PB.fromHandle h >-> hashBytes (hashInitWith MD5) >-> PP.stdoutLn
