module React.Registry where

import qualified Data.HashMap.Strict as H
import Data.IORef

import React.Imports

data RegistryStuff props state insig exsig = RegistryStuff
    { registryProps :: props
    , registryState :: state
    , registryHandler :: insig -> IO ()
    }


data ClassRegistry props state insig exsig = ClassRegistry
    { registryStuff :: IORef (H.HashMap Int (RegistryStuff props state insig exsig))
    , registryGen :: IORef Int
    }


generateKey :: ClassRegistry props state insig exsig -> IO Int
generateKey (ClassRegistry _ gen) = do
    k <- readIORef gen
    writeIORef gen (k + 1)
    return k


allocProps :: ClassRegistry props state insig exsig
           -> props
           -> IO Int
allocProps registry props = do
    k <- generateKey registry

    modifyIORef (registryStuff registry) $
        H.insert k (RegistryStuff props undefined undefined)
    return k


setHandler :: ClassRegistry props state insig exsig
           -> (insig -> IO ())
           -> Int
           -> IO ()
setHandler registry handler k =
    modifyIORef (registryStuff registry) $
        H.adjust (\(RegistryStuff p s _) -> RegistryStuff p s handler) k


setState :: ClassRegistry props state insig exsig -> state -> Int -> IO ()
setState registry state k =
    modifyIORef (registryStuff registry) $
        H.adjust (\(RegistryStuff p _ h) -> RegistryStuff p state h) k


deallocRegistry :: ClassRegistry props state insig exsig -> Int -> IO ()
deallocRegistry (ClassRegistry stuff _) k = modifyIORef stuff (H.delete k)


-- TODO(joel) - think about pushing around the IO boundary
lookupRegistry :: ClassRegistry props state insig exsig
               -> Int
               -> IO (RegistryStuff props state insig exsig)
lookupRegistry (ClassRegistry stuff _) k = do
    stuff' <- readIORef stuff
    return $ H.lookupDefault
        (error $ "class registry didn't contain an entry!\n" ++
                 "componentId: " ++ show k ++ "\n" ++
                 "keys: " ++ show (H.keys stuff') ++ "\n"
        )
        k
        stuff'
