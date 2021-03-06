{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances, FlexibleContexts, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Minimize
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Makes it possible to minimize windows, temporarily removing them
-- from the layout until they are restored.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Minimize (
        -- * Usage
        -- $usage
        minimize,
        MinimizeMsg(..)
    ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Layout.BoringWindows as BW
import Data.List
import qualified Data.Map as M

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Minimize
--
-- Then edit your @layoutHook@ by adding the Minimize layout modifier:
--
-- > myLayout = minimize (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In the key-bindings, do something like:
--
-- >        , ((modm,               xK_m     ), withFocused (\f -> sendMessage (MinimizeWin f)))
-- >        , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
--
-- The first action will minimize the focused window, while the second one will restore
-- the next minimized window.
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".
--
-- The module is designed to work together with "XMonad.Layout.BoringWindows" so
-- that minimized windows will be skipped over when switching the focused window with
-- the keyboard. Include 'BW.boringWindows' in your layout hook and see the
-- documentation of "XMonad.Layout.BoringWindows" on how to modify your keybindings.
--
-- Also see "XMonad.Hooks.RestoreMinimized" if you want to be able to restore
-- minimized windows from your taskbar.

data Minimize a = Minimize [Window] (M.Map Window W.RationalRect) deriving ( Read, Show )
minimize :: LayoutClass l Window => l Window -> ModifiedLayout Minimize l Window
minimize = ModifiedLayout $ Minimize [] (M.empty)

data MinimizeMsg = MinimizeWin Window
                    | RestoreMinimizedWin Window
                    | RestoreNextMinimizedWin
                    deriving (Typeable, Eq)
instance Message MinimizeMsg

instance LayoutModifier Minimize Window where
    modifierDescription (Minimize _ _) = "Minimize"

    modifyLayout (Minimize minimized _) wksp rect = do
        let stack = W.stack wksp
            filtStack = stack >>=W.filter (\w -> not (w `elem` minimized))
        runLayout (wksp {W.stack = filtStack}) rect

    handleMess (Minimize minimized unfloated) m
        | Just (MinimizeWin w) <- fromMessage m =
          if not (w `elem` minimized)
            then do
                BW.focusDown
                ws <- gets windowset
                case M.lookup w (W.floating ws) of
                  Nothing -> return $ Just $ Minimize (w:minimized) unfloated
                  Just r -> do
                    (windows . W.sink) w
                    return $ Just $ Minimize (w:minimized) (M.insert w r unfloated)

            else return Nothing
        | Just (RestoreMinimizedWin w) <- fromMessage m =
            case M.lookup w unfloated of
              Nothing -> return $ Just $ Minimize (minimized \\ [w]) unfloated
              Just r -> do
                (windows . (W.float w)) r
                return $ Just $ Minimize (minimized \\ [w]) (M.delete w unfloated)
        | Just RestoreNextMinimizedWin <- fromMessage m =
          if not (null minimized)
            then case M.lookup (head minimized) unfloated of
              Nothing -> do
                focus (head minimized)
                return $ Just $ Minimize (tail minimized) unfloated
              Just r -> do
                let w = head minimized
                (windows . (W.float w)) r
                focus w
                return $ Just $ Minimize (tail minimized) (M.delete w unfloated)
            else return Nothing
        | Just BW.UpdateBoring <- fromMessage m = do
            ws <- gets (W.workspace . W.current . windowset)
            flip sendMessageWithNoRefresh ws $ BW.Replace "Minimize" minimized
            return Nothing
        | otherwise = return Nothing
