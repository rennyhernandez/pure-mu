module KeyUtils (
createKeyring,
genUserKeyring,
generateAESKey,
) where

import Import
import Crypto.Types.PubKey.RSA
import Codec.Crypto.RSA
import Crypto.PasswordStore
import Crypto.Random  
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Codec.Binary.Base64.String as B64

createKeyring :: IO (PublicKey, PrivateKey)
createKeyring = do
    g <- liftIO $ newGenIO :: IO SystemRandom
    let (pubk, pvtk, _) = generateKeyPair g 2048  
    return (pubk, pvtk)

genUserKeyring :: IO (PublicKey, PrivateKey, SystemRandom)
genUserKeyring = do 
  g <- newGenIO :: IO SystemRandom
  let (pubk, pvtk, gen) = generateKeyPair g 2048 -- for RSA 2048 use 
  return $ (pubk,pvtk,gen)

generateAESKey :: IO B.ByteString
generateAESKey = do 
  g <- newGenIO :: IO SystemRandom
  let res = genBytes 32 g -- 32 * 8  = 256 bits
  case res of
    Right (x, _) -> return x
    Left  _      -> error "Couldn't generate AES Key"
