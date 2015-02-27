{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (FilePath)

import Control.Concurrent.MVar
import Control.Concurrent.Async

import Control.Monad

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.NodeMap hiding (run, run_)

import Data.List
import Data.Maybe
import Data.Tree

import qualified Data.Map  as M
import qualified Data.Text as T

import Shelly

default (T.Text)

-- Definitions for parallel job execution:

type Job = MVar (Either (Sh (Async ())) (Async ()))

type JobTag      = String
type JobMap      = M.Map String (Sh ())
type JobDepends  = (JobTag, JobTag, ())
type JobTagGraph = Gr String ()
type JobGraph    = Gr Job ()

newMVarSh :: a -> Sh (MVar a)
newMVarSh = liftIO . newMVar

takeMVarSh :: MVar a -> Sh a
takeMVarSh = liftIO . takeMVar

putMVarSh :: MVar a -> a -> Sh ()
putMVarSh = (liftIO . ) . putMVar

readMVarSh :: MVar a -> Sh a
readMVarSh = liftIO . readMVar

waitSh :: Async a -> Sh a
waitSh = liftIO . wait

mkJob :: Sh () -> Sh Job
mkJob = newMVarSh . Left . asyncSh

require :: a -> b -> (a, b, ())
require = flip flip () . (,,)

sequenceNodes :: (DynGraph gr, Monad m) => gr (m a) b -> m (gr a b)
sequenceNodes = ufold sequenceContext (return empty)
    where sequenceContext (ine, n, nl, oute) g = do
            nl' <- nl
            g'  <- g
            return $ (ine, n, nl', oute) & g'

mkJobTagGraph :: [JobDepends] -> JobTagGraph
mkJobTagGraph js = fst $ mkMapGraph ujs js
    where ujs = nub $ foldr (\(a,b,()) ls -> a:b:ls) [] js

mkJobGraph :: JobMap -> JobTagGraph -> Sh JobGraph
mkJobGraph m = sequenceNodes . nmap (mkJob . (m M.!))

blockOn :: Job -> Sh ()
blockOn j = readMVarSh j >>= \case Left _  -> error "Cannot block on job that has not started."
                                   Right a -> waitSh a

runJob :: JobGraph -> Node -> Sh ()
runJob g n = do
    let j = fromJust $ lab g n
    readMVarSh j >>= \case Right _ -> return ()
                           Left  _ -> do
                             let deps = suc g n
                             mapM_ (runJob g) deps
                             mapM_ blockOn $ map (fromJust . lab g) deps
                             s <- takeMVarSh j
                             case s of Right _ -> putMVarSh j s
                                       Left sh -> sh >>= putMVarSh j . Right

runJobGraph :: JobMap -> JobTagGraph -> Sh ()
runJobGraph m g = do
    g' <- mkJobGraph m g
    let js = nodes g'
    mapM_ (runJob g') js
    mapM_ blockOn $ map (fromJust . lab g') js

-- Static relative paths to executables:

whoami_path :: FilePath
whoami_path = "whoami"

wget_path :: FilePath
wget_path = "wget"

wget :: T.Text -> Sh ()
wget = run_ wget_path . ("-t" :) . ("0" :) . (:[])

rpm_path :: FilePath
rpm_path = "rpm"

yum_path :: FilePath
yum_path = "yum"

yum_update :: Sh ()
yum_update = run_ yum_path ["-y", "update"]

yum_upgrade :: Sh ()
yum_upgrade = run_ yum_path ["-y", "upgrade"]

yum_install :: [T.Text] -> Sh ()
yum_install = run_ yum_path . ("-y" :) . ("install" :)

ldconfig_path :: FilePath
ldconfig_path = "/sbin/ldconfig"

tar_path :: FilePath
tar_path = "tar"

make_path :: FilePath
make_path = "make"

perl_path :: FilePath
perl_path = "perl"

-- Static config paths

ld_local_path :: FilePath
ld_local_path = "/etc/ld.so.conf.d/local.conf"

pkg_config_var :: T.Text
pkg_config_var = "PKG_CONFIG_PATH"

pkg_config_val :: T.Text
pkg_config_val = "/usr/lib64/pkgconfig:/usr/share/pkgconfig:/usr/local/lib/pkgconfig:/usr/local/share/pkgconfig"

-- Static external paths:

epel_name = "epel-release-6-8.noarch.rpm"
epel_path = "http://download.fedoraproject.org/pub/epel/6/x86_64/" `T.append` epel_name

raptor_dir = "raptor2-2.0.14"
raptor_tar = raptor_dir `T.append` ".tar.gz"
raptor_path = "http://download.librdf.org/source/" `T.append` raptor_tar

rasqal_dir = "rasqal-0.9.32"
rasqal_tar = rasqal_dir `T.append` ".tar.gz"
rasqal_path = "http://download.librdf.org/source/" `T.append` rasqal_tar

fstore_dir = "4store-v1.1.5"
fstore_tar = fstore_dir `T.append` ".tar.gz"
fstore_path = "http://4store.org/download/" `T.append` fstore_tar

hdf5_dir = "hdf5-1.8.14"
hdf5_tar = hdf5_dir `T.append` ".tar.gz"
hdf5_path = "http://www.hdfgroup.org/ftp/HDF5/current/src/" `T.append` hdf5_tar

zmq_dir = "zeromq-4.0.5"
zmq_tar = zmq_dir `T.append` ".tar.gz"
zmq_path = "http://download.zeromq.org/" `T.append` zmq_tar

ghc_link1_dir = "ghc-7.4.1-src"
ghc_link1_tar = ghc_link1_dir `T.append` ".tar.bz2"
ghc_link1_path = "http://www.haskell.org/ghc/dist/7.4.1/" `T.append` ghc_link1_tar

ghc_link2_dir = "ghc-7.8.4-src"
ghc_link2_tar = ghc_link2_dir `T.append` ".tar.bz2"
ghc_link2_path = "http://www.haskell.org/ghc/dist/7.8.4/" `T.append` ghc_link2_tar

-- List of all RPMs we're going to need:

allRPMs :: [T.Text]
allRPMs = [
    "tmux"
   ,"htop"
   ,"clang"
   ,"gcc"
   ,"autoconf"
   ,"libtool"
   ,"make"
   ,"valgrind"
   ,"gmp"
   ,"gmp-devel"
   ,"gmp-static"
   ,"mpfr"
   ,"mpfr-devel"
   ,"git"
   ,"nasm"
   ,"python33"
   ,"texlive-latex"
   ,"gnuplot"
   ,"R"
   ,"nodejs"
   ,"npm"
   ,"php"
   ,"nmap"
   ,"ocaml"
   ,"glib2"
   ,"glib2-devel"
   ,"libxml2"
   ,"libxml2-devel"
   ,"pcre"
   ,"pcre-devel"
   ,"avahi"
   ,"avahi-glib-devel"
   ,"avahi-libs"
   ,"avahi-devel"
   ,"avahi-tools"
   ,"readline"
   ,"readline-devel"
   ,"ncurses"
   ,"ncurses-libs"
   ,"ncurses-devel"
   ,"expat"
   ,"expat-devel"
   ,"zlib"
   ,"zlib-devel"
   ,"libcurl"
   ,"libcurl-devel"
   ,"libxslt"
   ,"libxslt-devel"
   ,"uuid"
   ,"libuuid"
   ,"libuuid-devel"
   ,"happy"
   ,"alex"
   ,"ghc"
   ]

-- Job definition:

initPKUtree :: Sh ()
initPKUtree = mkdirTree tree
    where (#)    = Node
          tree = "/app" # [
            "Analytics" # []
           ,"Datablocks" # [
                "jnj28431754" # []
               ,"jnj38969216_remicade_c168" # []
               ,"jnj54767414_dara" # []
               ,"jnj56021927_gainer" # []
               ,"jnj56022473_castle" # []
               ]
           ,"sandboxes" # []
           ,"schema_files" # []
           ,"source_systems" # [
                "edw" # []
               ,"ODS" # []
               ,"OLTP" # []
               ,"Symyx_dev" # []
               ,"Symyx_prod" # []
               ]
           ,"src" # [
                "lang" # [
                    "C" # []
                   ,"Haskell" # []
                   ]
               ,"pkg" # [
                    "4store" # []
                   ,"czmq" # []
                   ,"zmq" # []
                   ,"ghc_bootstrap" # []
                   ,"hdf5" # []
                   ,"libsodium" # []
                   ,"php-zmq" # []
                   ,"R" # []
                   ,"raptor" # []
                   ,"rasqal" # []
                   ]
               ]
           ,"sync_scripts" # []
           ]

pullEpel :: Sh ()
pullEpel = do
    cd "/app"
    rm_rf $ fromText epel_name
    wget epel_path

pullRaptor :: Sh ()
pullRaptor = do
    cd "/app/src/pkg/raptor"
    rm_rf $ fromText raptor_dir
    rm_rf $ fromText raptor_tar
    wget raptor_path

pullRasqal :: Sh ()
pullRasqal = do
    cd "/app/src/pkg/rasqal"
    rm_rf $ fromText rasqal_dir
    rm_rf $ fromText rasqal_tar
    wget rasqal_path

pullFstore :: Sh ()
pullFstore = do
    cd "/app/src/pkg/4store"
    rm_rf $ fromText fstore_dir
    rm_rf $ fromText fstore_tar
    wget fstore_path

pullHDF5 :: Sh ()
pullHDF5 = do
    cd "/app/src/pkg/hdf5"
    rm_rf $ fromText hdf5_dir
    rm_rf $ fromText hdf5_tar
    wget hdf5_path

pullZmq :: Sh ()
pullZmq = do
    cd "/app/src/pkg/zmq"
    rm_rf $ fromText zmq_dir
    rm_rf $ fromText zmq_tar
    wget zmq_path

pullGHC1 :: Sh ()
pullGHC1 = do
    cd "/app/src/pkg/ghc_bootstrap"
    rm_rf $ fromText ghc_link1_dir
    rm_rf $ fromText ghc_link1_tar
    wget ghc_link1_path

pullGHC2 :: Sh ()
pullGHC2 = do
    cd "/app/src/pkg/ghc_bootstrap"
    rm_rf $ fromText ghc_link2_dir
    rm_rf $ fromText ghc_link2_tar
    wget ghc_link2_path

initYum :: Sh ()
initYum = do
    cd "/app"
    yum_update
    yum_upgrade
    yum_install ["curl", "nss"]
    run_ rpm_path ["--force", "-ivh", epel_name]
    yum_update
    yum_upgrade
    yum_install allRPMs

-- These installations must share an Sh environment
raptorRasqalFstoreInstall :: Sh ()
raptorRasqalFstoreInstall = do
    appendfile ld_local_path "/usr/local/lib\n/usr/local/lib64\n"
    run_ ldconfig_path []
    setenv pkg_config_var pkg_config_val
    cd "/app/src/pkg/raptor"
    run_ tar_path ["xvzf", raptor_tar]
    cd $ fromText raptor_dir
    run_ "./configure" []
    run_ make_path []
    run_ make_path ["install"]
    run_ ldconfig_path []
    cd "/app/src/pkg/rasqal"
    run_ tar_path ["xvzf", rasqal_tar]
    cd $ fromText rasqal_dir
    run_ "./configure" []
    run_ make_path []
    run_ make_path ["install"]
    run_ ldconfig_path []
    cd "/app/src/pkg/4store"
    run_ tar_path ["xvzf", fstore_tar]
    cd $ fromText fstore_dir
    run_ "./configure" ["--with-storage-path=/app/4s-data"]
    run_ make_path []
    run_ make_path ["install"]
    run_ ldconfig_path []

hdf5Install :: Sh ()
hdf5Install = do
    cd "/app/src/pkg/hdf5"
    run_ tar_path ["xvzf", hdf5_tar]
    cd $ fromText hdf5_dir
    run_ "./configure" ["--prefix=/usr/local"]
    run_ make_path []
    run_ make_path ["install"]
    run_ ldconfig_path []

zmqInstall :: Sh ()
zmqInstall = do
    cd "/app/src/pkg/zmq"
    run_ tar_path ["xvzf", zmq_tar]
    cd $ fromText zmq_dir
    run_ "./configure" []
    run_ make_path []
    run_ make_path ["install"]
    run_ ldconfig_path []

ghcLink1 :: Sh ()
ghcLink1 = do
    cd "/app/src/pkg/ghc_bootstrap"
    run_ tar_path ["xvjf", ghc_link1_tar]
    cd $ fromText ghc_link1_dir
    run_ perl_path ["boot"]
    run_ "./configure" []
    run_ make_path []
    run_ make_path ["install"]

ghcLink2 :: Sh ()
ghcLink2 = do
    cd "/app/src/pkg/ghc_bootstrap"
    run_ tar_path ["xvjf", ghc_link2_tar]
    cd $ fromText ghc_link2_dir
    run_ perl_path ["boot"]
    run_ "./configure" []
    run_ make_path []
    run_ make_path ["install"]

depmap = M.fromList [
    ("initPKUtree", initPKUtree)
   ,("pullEpel", pullEpel)
   ,("pullRaptor", pullRaptor)
   ,("pullRasqal", pullRasqal)
   ,("pullFstore", pullFstore)
   ,("pullHDF5", pullHDF5)
   ,("pullZmq", pullZmq)
   ,("pullGHC1", pullGHC1)
   ,("pullGHC2", pullGHC2)
   ,("initYum", initYum)
   ,("raptorRasqalFstoreInstall", raptorRasqalFstoreInstall)
   ,("hdf5Install", hdf5Install)
   ,("zmqInstall", zmqInstall)
   ,("ghcLink1", ghcLink1)
   ,("ghcLink2", ghcLink2)
   ]

depgraph= mkJobTagGraph [
    ("pullEpel" `require` "initPKUtree")
   ,("pullRaptor" `require` "initPKUtree")
   ,("pullRasqal" `require` "initPKUtree")
   ,("pullFstore" `require` "initPKUtree")
   ,("pullHDF5" `require` "initPKUtree")
   ,("pullZmq" `require` "initPKUtree")
   ,("pullGHC1" `require` "initPKUtree")
   ,("pullGHC2" `require` "initPKUtree")
   ,("initYum" `require` "pullEpel")
   ,("raptorRasqalFstoreInstall" `require` "initYum")
   ,("raptorRasqalFstoreInstall" `require` "pullRaptor")
   ,("raptorRasqalFstoreInstall" `require` "pullRasqal")
   ,("raptorRasqalFstoreInstall" `require` "pullFstore")
   ,("hdf5Install" `require` "pullHDF5")
   ,("zmqInstall" `require` "pullZmq")
   ,("ghcLink1" `require` "pullGHC1")
   ,("ghcLink2" `require` "pullGHC2")
   ,("ghcLink2" `require` "ghcLink1")
   ]

preflight :: Sh [T.Text]
preflight = liftM catMaybes checks
    where checks = sequence [
            test_px whoami_path >>= (\e -> return $ if e then Nothing else Just $ "whoami program not found in $PATH.")
           ,run whoami_path [] >>= (\u -> return $ if (u == "root\n") then Nothing else Just $ "This program must be executed as root.")
           ,test_px wget_path >>= (\e -> return $ if e then Nothing else Just $ "wget program not found in $PATH.")
           ,test_px rpm_path >>= (\e -> return $ if e then Nothing else Just $ "rpm program not found in $PATH.")
           ,test_px yum_path >>= (\e -> return $ if e then Nothing else Just $ "yum program not found in $PATH.")
           ,test_px ldconfig_path >>= (\e -> return $ if e then Nothing else Just $ "/sbin/ldconfig program not found.")
           ,test_px tar_path >>= (\e -> return $ if e then Nothing else Just $ "tar program not found in $PATH.")
           ,test_px make_path >>= (\e -> return $ if e then Nothing else Just $ "make program not found in $PATH.")
           ,test_px perl_path >>= (\e -> return $ if e then Nothing else Just $ "perl program not found in $PATH.")
--           ,test_f ld_local_path >>= (\e -> return $ if e then Nothing else Just $ "/etc/ld.so.conf.d/local.conf not found.")
           ]

main = shelly $ do
    echo_n "Performing pre-flight check... "
    preflight >>= \case [] -> echo "Success."
                        es -> echo "Failure." >> echo (T.intercalate "\n" es) >> quietExit 1
    runJobGraph depmap depgraph
