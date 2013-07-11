{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances, PatternGuards #-}
module XMonad.Layout.XkbLayout (xkbLayout) where

import XMonad
import qualified Data.Map as M
import XMonad.StackSet hiding (workspaces)

foreign import ccall unsafe "XkbLockGroup" xkbLockGroup :: Display -> XID -> XID ->IO ()

data XkbToggle = XkbToggle (Maybe XID) deriving ( Typeable )
instance Message XkbToggle
data XkbLayout l a = XkbLayout XID (M.Map Window XID) (l a) deriving ( Read, Show )

xkbLayout = XkbLayout 0 M.empty

instance (Ord a, LayoutClass l a) => LayoutClass (XkbLayout l) a where
    runLayout (Workspace tag m@(XkbLayout defXkbGroup xkbWinMap l) ms) r =
        do withWindowSet $ \ws ->
               if tag == currentTag ws
               then whenJust (peek ws) (lockGroup m)
               else return ()
           (ws, l') <- runLayout (Workspace tag l ms) r
           case l' of
             Nothing -> return (ws, Nothing)
             Just l -> return (ws, Just (XkbLayout defXkbGroup xkbWinMap l))

    handleMessage xkb@(XkbLayout defXkbGroup xkbWinMap l) m
        | Just (XkbToggle group) <- fromMessage m = withWindowSet (return . peek) >>= maybe (return Nothing) (toggleGroup xkb group)
        | otherwise = (handleMessage l m) >>= maybe (return Nothing) (\l -> return $ Just (XkbLayout defXkbGroup xkbWinMap l))

    description (XkbLayout _ _ l) = description l

xkbGetGroup :: (Ord b) => XkbLayout a b -> Window -> XID
xkbGetGroup (XkbLayout d m _) w  = M.findWithDefault d w m


lockGroup :: (Ord b) => XkbLayout a b -> Window -> X ()
lockGroup l@(XkbLayout d m _) w = withDisplay $ \dpy -> io $ xkbLockGroup dpy 0x100 (xkbGetGroup l w)

toggleGroup :: (Ord b) => XkbLayout a b -> Maybe XID -> Window -> X (Maybe (XkbLayout a b))
toggleGroup l@(XkbLayout d m l') (Just g) w = return $ Just (XkbLayout d (M.insert w g m) l')
toggleGroup l@(XkbLayout d m l') Nothing w =
    do XConf { display = dpy, theRoot = root } <- ask
       classHint <- io $ getClassHint dpy w
       if resClass classHint == "Emacs"
        then do io $ allocaXEvent $ \ev ->
                   do setEventType ev keyPress
                      keyCode <- keysymToKeycode dpy xK_backslash
                      setKeyEvent ev w root 0 controlMask keyCode True
                      sendEvent dpy w False 0 ev
                return Nothing
        else return $ Just (XkbLayout d (M.insert w (flop $ xkbGetGroup l w) m) l')
               where flop 0 = 1
                     flop _ = 0

