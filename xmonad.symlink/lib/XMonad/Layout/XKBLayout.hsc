{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface, MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- GHC 6.10.4 complains about Foreign.C.Types, see Ticket #3419

-- Version 2
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.XKBLayout
-- Copyright   :  (c) Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that remembers per-window keyboard layouts and switches them
-- on focus changes.
--
-----------------------------------------------------------------------------

module XMonad.Layout.XKBLayout (
                                -- * Usage
                                -- $usage
                                xkbLayout) where

import Foreign
import Foreign.Ptr
import Foreign.C.Types (CUChar,CUShort,CUInt,CInt)

import Graphics.X11.Xlib.Types
import qualified Data.Map as M
import Data.List (find)
import Control.Monad

import XMonad
import XMonad.Core (whenJust)
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Util.StringProp (StringProp,getStringProp,setStringProp)

#include <X11/XKBlib.h>
#include <X11/extensions/XKBstr.h>

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.XKBLayout
--
-- Then edit your @layoutHook@ by adding 'xkbLayout', for example
--
-- > myLayout = xkbLayout $ Tall 1 (3/100) (1/2) ||| Full || etc..
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- For more instructions on editing the @layoutHook@ see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data XkbStateRec = XkbStateRec {
    group :: CUChar,
    locked_group :: CUChar,
    base_group :: CUShort,
    latched_group :: CUShort,
    mods :: CUChar,
    base_mods :: CUChar,
    latched_mods :: CUChar,
    locked_mods :: CUChar,
    compat_state :: CUChar,
    grab_mods :: CUChar,
    compat_grab_mods :: CUChar,
    lookup_mods :: CUChar,
    compat_lookup_mods :: CUChar,
    ptr_buttons :: CUShort
}

instance Storable XkbStateRec where
    sizeOf _ = (#size XkbStateRec)
    alignment _ = alignment (undefined :: CUShort)
    peek ptr = do
        r_group <- (#peek XkbStateRec, group) ptr
        r_locked_group <- (#peek XkbStateRec, locked_group) ptr
        r_base_group <- (#peek XkbStateRec, base_group) ptr
        r_latched_group <- (#peek XkbStateRec, latched_group) ptr
        r_mods <- (#peek XkbStateRec, mods) ptr
        r_base_mods <- (#peek XkbStateRec, base_mods) ptr
        r_latched_mods <- (#peek XkbStateRec, latched_mods) ptr
        r_locked_mods <- (#peek XkbStateRec, locked_mods) ptr
        r_compat_state <- (#peek XkbStateRec, compat_state) ptr
        r_grab_mods <- (#peek XkbStateRec, grab_mods) ptr
        r_compat_grab_mods <- (#peek XkbStateRec, compat_grab_mods) ptr
        r_lookup_mods <- (#peek XkbStateRec, lookup_mods) ptr
        r_compat_lookup_mods <- (#peek XkbStateRec, compat_lookup_mods) ptr
        r_ptr_buttons <- (#peek XkbStateRec, ptr_buttons) ptr
        return XkbStateRec {
            group = r_group,
            locked_group = r_locked_group,
            base_group = r_base_group,
            latched_group = r_latched_group,
            mods = r_mods,
            base_mods = r_base_mods,
            latched_mods = r_latched_mods,
            locked_mods = r_locked_mods,
            compat_state = r_compat_state,
            grab_mods = r_grab_mods,
            compat_grab_mods = r_compat_grab_mods,
            lookup_mods = r_lookup_mods,
            compat_lookup_mods = r_compat_lookup_mods,
            ptr_buttons = r_ptr_buttons
        }

foreign import ccall unsafe "X11/XKBlib.h XkbGetState"
    xkbGetState :: Display -> CUInt -> Ptr XkbStateRec -> IO CInt

foreign import ccall unsafe "XkbLockGroup" xkbLockGroup :: Display -> XID -> XID -> IO ()

type KbdLayout = Int

getKbdLayout :: Display -> IO KbdLayout
getKbdLayout d = alloca $ \stRecPtr -> do
    xkbGetState d 0x100 stRecPtr
    st <- peek stRecPtr
    return $ fromIntegral (group st)

setKbdLayout :: Display -> KbdLayout -> IO ()
setKbdLayout d l = xkbLockGroup d 0x100 $ fromIntegral l

data UpdateMessage = UpdateMessage WorkspaceId deriving (Typeable)
instance Message UpdateMessage

maybeM :: Monad m => b -> (a -> m b) -> Maybe a -> m b
maybeM d _ Nothing = return d
maybeM _ f (Just a) = f a

prevWkTagProp :: StringProp
prevWkTagProp = "prev_wksp_tag"

getPrevWksp :: Display -> X (W.Workspace WorkspaceId (Layout Window) Window)
getPrevWksp dpy = do
    mtags <- getStringProp dpy prevWkTagProp
    cur <- gets (W.workspace . W.current . windowset)
    maybeM cur (\tags ->
        gets (W.workspaces . windowset) >>= \wspaces ->
        return $ maybe cur id (find (\ws -> (read tags) == (W.tag ws)) wspaces)) mtags

setCurWksp :: Display -> X ()
setCurWksp dpy = do
    wk <- gets (W.workspace . W.current . windowset)
    setStringProp dpy prevWkTagProp (show $ W.tag wk)

data XKBLayout a = XKBLayout (Maybe Window) (M.Map Window KbdLayout) deriving (Read, Show)

saveKbdLayout :: Display -> XKBLayout a -> Window -> X (XKBLayout a)
saveKbdLayout dpy (XKBLayout mlf lts) foc = do
    curLayout <- io $ getKbdLayout dpy
    let lts' = maybe lts (\lf -> M.insert lf curLayout lts) mlf
    alive <- filterM isClient (M.keys lts')
    let lts'' = M.filterWithKey (\w -> \_ -> w `elem` alive) lts'
    return $ XKBLayout (Just foc) lts''

restoreKbdLayout :: Display -> XKBLayout a -> Window -> X ()
restoreKbdLayout dpy (XKBLayout _ lts) foc = io $ whenJust (M.lookup foc lts) (setKbdLayout dpy)

update :: XKBLayout a -> X (Maybe (XKBLayout a))
update l = withDisplay $ \dpy -> do
    prevWk <- getPrevWksp dpy
    curWk <- gets (W.workspace . W.current . windowset)
    let mst = W.stack curWk
    let mf = maybe Nothing (Just . W.focus) mst
    if (W.tag prevWk) == (W.tag curWk)
        then maybeM Nothing (\foc -> updateC dpy l foc >>= return.Just) mf
        else do
            sendMessageWithNoRefresh (UpdateMessage (W.tag prevWk)) prevWk
            setCurWksp dpy
            maybeM Nothing (\foc -> (restoreKbdLayout dpy l foc) >> return Nothing) mf
    where
        updateC :: Display -> XKBLayout a -> Window -> X (XKBLayout a)
        updateC dpy l' foc = do
            setCurWksp dpy
            l'' <- saveKbdLayout dpy l' foc
            restoreKbdLayout dpy l' foc
            return l''

instance LayoutModifier XKBLayout a where
    handleMess l m
        | Just (UpdateMessage wid) <- fromMessage m = withDisplay $ \dpy -> do
            wspaces <- gets (W.workspaces . windowset)
            let mws = find (\ws -> wid == (W.tag ws)) wspaces
            let mst = maybe Nothing W.stack mws
            maybeM Nothing (\foc -> (saveKbdLayout dpy l foc) >>= return.Just)
                           (maybe Nothing (Just . W.focus) mst)
    handleMess _ _ = return Nothing
    redoLayout l _ _ wrs = update l >>= \l' -> return (wrs, l')

xkbLayout :: l a -> ModifiedLayout XKBLayout l a
xkbLayout = ModifiedLayout $ XKBLayout Nothing M.empty

-- vim:ft=haskell:ts=4:shiftwidth=4:softtabstop=4:expandtab:foldlevel=20:
