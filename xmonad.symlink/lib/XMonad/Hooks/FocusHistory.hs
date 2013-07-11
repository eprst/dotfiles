{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.FocusHistory
-- Copyright   :  (c) Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A hook that tracks focus history and allows to switch focus back and
-- forward.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.FocusHistory (
        -- * Usage
        -- $usage
        focusHistoryHook,
        focusHistoryBack,
        focusHistoryForward
) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import Control.Monad (when)
import Data.Maybe
import Data.Monoid

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.FocusHistory
--
-- then add it to your handleEventHook:
--
-- > handleEventHook = ... <+> focusHistory 10
--
-- where 10 is history size.
--
-- Use @focusHistoryBack@ and @focusHistoryForward@ actions to
-- focus back and forward in history.
-- Here's an example with Windows-like keybindings:
--
-- >   , ((mod1Mask, xK_Tab),                focusHistoryBack)    -- Alt+Tab to go back
-- >   , ((mod1Mask .|. shiftMask, xK_Tab),  focusHistoryForward) -- Alt+Shift+Tab to go forward
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data FocusHistory = FocusHistory {mlim :: Maybe Int, mhd :: Maybe (W.Stack Window)} deriving (Typeable, Read, Show)
instance ExtensionClass FocusHistory where initialValue = FocusHistory Nothing Nothing

-- add window to history, keeping size limit in mind
addToHistory :: Window -> FocusHistory -> FocusHistory
addToHistory _ (FocusHistory Nothing _) = FocusHistory Nothing Nothing
addToHistory _ (FocusHistory (Just 0) _) = FocusHistory (Just 0) Nothing
addToHistory w fhd@(FocusHistory _ Nothing) = fhd{mhd = (W.differentiate [w])}
addToHistory w fhd@(FocusHistory (Just lim) (Just (W.Stack x _ r))) = fhd{mhd = Just (W.Stack w [] (take lim (x:r)))}

-- | History tracking hook
focusHistoryHook :: Int    -- ^ History size limit
                 -> Event -> X All
focusHistoryHook lim _ = do
        fhd <- XS.get
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let focusChanged = fromMaybe True $ do
                cf <- mst
                hf <- mhd fhd
                return $ (W.focus cf) /= (W.focus hf)
        when focusChanged $ do
                XS.put $ addToHistory (W.focus (fromJust mst)) fhd{mlim = Just lim}
        return (All True)

-- | Focus previous window in history
focusHistoryBack :: X()
focusHistoryBack = changeFocus W.focusDown'

-- | Focus next window in history
focusHistoryForward :: X()
focusHistoryForward = changeFocus W.focusUp'

changeFocus :: (W.Stack Window -> W.Stack Window) -> X()
changeFocus op = (XS.get :: X FocusHistory) >>= changeFocus'
        where
        changeFocus' (FocusHistory Nothing _) = return ()
        changeFocus' (FocusHistory _ Nothing) = return ()
        changeFocus' fhd@(FocusHistory _ (Just h)) = let newh = op h in do
                XS.put $ fhd{mhd = Just newh}
                focus $ W.focus newh

-- vim ts=4:shiftwidth=4:softtabstop=4:expandtab:foldlevel=20:
