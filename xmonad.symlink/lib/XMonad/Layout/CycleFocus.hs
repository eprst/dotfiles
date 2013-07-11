{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.CycleFocus
-- Copyright   :  (c) Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier that allows to cycle focused window inside the wrapped
-- layout.
--
-----------------------------------------------------------------------------
module XMonad.Layout.CycleFocus {- (
                                 -- * Usage
                                 -- $usage
                                 MoveFocus(..),
                                 cycleFocus
                                ) -} where

import XMonad hiding (focus)
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

import Data.List

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.CycleFocus
-- > import XMonad.Layout.LayoutCombinators
--
-- now having something like
--
-- > (cycleFocus simpleTabbed) *|* (cycleFocus simpleTabbed)
--
-- in your layout you'll be able to cycle focus
-- separately in every pane.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- use 'FocusNext' and 'FocusPrev' events to cycle focus, you can do it
-- with the following keybindings:
--
-- >    , ((modMask x, xK_Right), sendMessage FocusNext)
-- >    , ((modMask x, xK_Left ), sendMessage FocusPrev)
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data MoveFocus =   FocusNext  -- ^ Focus next window
                 | FocusPrev  -- ^ Focus previous window
                 deriving (Read, Show, Typeable)
instance Message MoveFocus

-- here we store a list of managed windows, updated in pureModifier
data CycleFocus a = CycleFocus [a] deriving (Read, Show)

instance LayoutModifier CycleFocus Window where
    modifierDescription _ = "CycleFocus"

    pureModifier _ _ mst wrs = (wrs, Just $ CycleFocus $ W.integrate' mst) -- update state

    -- todo: with such design two nested CycleFocus instances will hanlde
    -- messages twice
    handleMess (CycleFocus w) m
        | Just FocusNext <- fromMessage m = windows (W.modify' $ cycl W.focusDown' w) >> return Nothing
        | Just FocusPrev <- fromMessage m = windows (W.modify' $ cycl W.focusUp'   w) >> return Nothing
        | otherwise = return Nothing


cycl :: (Eq a) => (W.Stack a -> W.Stack a) -> [a] -> W.Stack a -> W.Stack a
cycl f w st = applyUntil f (\s -> (W.focus s) `elem` w) st
    where
    applyUntil :: (a -> a) -> (a -> Bool) -> a -> a
    applyUntil g p a = head $ filter p $ tail $ iterate g a

-- | Modifier which adds focus cycling to the given layout
cycleFocus :: (LayoutClass l a, Eq a) => l a -> ModifiedLayout CycleFocus l a
cycleFocus = ModifiedLayout (CycleFocus [])

-- | Turn a list into a stack with specified focused elements. Order of elements
-- is preserved.
diff :: (Eq a) => [a] -> a -> Maybe (W.Stack a)
diff as f = (elemIndex f as) >>= \i -> Just $ W.Stack f (take i as) (drop (i+1) as)

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:foldlevel=20:
