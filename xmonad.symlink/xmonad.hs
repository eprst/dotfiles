{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternGuards, ScopedTypeVariables #-}

-- TODO: meta-k not creating new terminal; try new layout from contrib

import XMonad hiding ( (|||), focus )
import XMonad.Actions.TagWindows
import XMonad.Actions.WindowGo
import XMonad.Actions.Submap
import XMonad.Actions.WindowNavigation
import XMonad.Prompt
import XMonad.Actions.Search hiding (Query)
import XMonad.Actions.OnScreen (viewOnScreen, onlyOnScreen)
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll
--import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Config.Desktop
--import XMonad.Config.Gnome
import XMonad.Config.Xfce
import XMonad.Layout.CycleFocus
--import XMonad.Layout.OnWindowSubset
import XMonad.Layout.DragPane
import XMonad.Layout.LayoutModifier
import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ComboP
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.WindowNavigation ( MoveWindowToWindow(..) )
import XMonad.Layout.Minimize
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Spiral
import XMonad.StackSet ( integrate, Workspace (..), Stack(..) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FocusHistory
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.PerWindowKbdLayout
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.WindowProperties
import XMonad.Util.Run
import XMonad.Util.XSelection
import XMonad.Util.WorkspaceCompare
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell(shellPrompt)
import XMonad.Prompt.Input
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- import Graphics.X11.Xlib
-- import Graphics.X11.Xlib.Extras
import Foreign.C.Types (CLong(..))
import Codec.Binary.UTF8.String

import System.IO
import Data.List
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.String.Utils
import Control.Monad
import Control.Exception

--import DBus
--import DBus.Connection
--import DBus.Message

ignoreIOErrors :: IO () -> IO ()
ignoreIOErrors x = catch x (\(e :: SomeException) -> return ())

logEventHook :: Event -> X All
logEventHook e =
    case e of
        PropertyEvent {} -> return (All True)
        _ -> foc >>= klog >> return (All True)
    where
        foc :: X String
        foc = withDisplay $ \dpy -> do
            (w, _) <- io $ getInputFocus dpy
            t <- runQuery title w
            n <- runQuery Main.name w
            c <- runQuery clas w
            return $ "("++(show w)++"): wm_class="++c++", title="++t++", wm_name="++n

data DumpWindows a = DumpWindows (Maybe a) deriving (Read, Show) -- we store last focus seen in the stack
instance LayoutModifier DumpWindows Window where
    redoLayout _ _ Nothing wrs = return (wrs, Nothing)
    redoLayout _ _ (Just st) wrs = do
        u <- (runQuery title) `mapM` (up st)
        d <- (runQuery title) `mapM` (down st)
        f <- runQuery title (W.focus st)
        let t = 7
        let ut = (take t) `map` u
        let dt = (take t) `map` d
        let ft = take t f
        klog $ (show (reverse ut)) ++ " >" ++ ft ++ "< " ++ (show dt)
        withFocused $ \rf -> do
            rft <- runQuery title rf
            nonf <- runQuery isNotFloating rf
            klog $ "real focus: " ++ (take t rft) ++ "; floating: " ++ (show $ not nonf)
        return (wrs, Nothing)

    modifyLayout (DumpWindows last_focus) ws r = runLayout ws r

data FixFocus a = FixFocus (Maybe a) deriving (Read, Show)
instance LayoutModifier FixFocus Window where
    modifyLayout (FixFocus mlf) ws@(Workspace id lay Nothing) r = runLayout ws r
    modifyLayout (FixFocus Nothing) ws r = runLayout ws r
    modifyLayout (FixFocus (Just lf)) (Workspace id lay (Just st)) r = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        is_rf_floating <- maybe (return False) (\rf -> withWindowSet $ return . M.member rf . W.floating) mreal_f -- real focused window is floating?
        let new_stack_f = if is_rf_floating then lf else stack_f --if yes: replace stack's focus with our last saved focus
        let new_st' = until (\s -> new_stack_f == W.focus s) W.focusUp' st -- new stack with focused new_stack_f
        let new_st = if (new_stack_f `elem` (integrate st)) then new_st' else st -- use it only when it's possible
        runLayout (Workspace id lay (Just new_st)) r

    -- redoLayout (FixFocus mlf) r Nothing wrs = return (wrs, Just $ FixFocus mlf)
    redoLayout (FixFocus mlf) r Nothing wrs = return (wrs, Nothing)
    redoLayout (FixFocus mlf) r (Just st) wrs = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        let crf_in_stack = maybe False ((flip elem) (integrate st)) mreal_f -- current real focus belongs to stack?
        let new_saved_f = if crf_in_stack then fromJust mreal_f else stack_f -- if yes: replace saved focus
        return (wrs, Just $ FixFocus $ Just new_saved_f)

fixFocus = ModifiedLayout $ FixFocus Nothing

-------------------

-- Send WM_TAKE_FOCUS
-- takeTopFocus = withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek
takeTopFocus = (return ())

-- atom_WM_TAKE_FOCUS      = getAtom "WM_TAKE_FOCUS"
-- takeFocusX w = withWindowSet $ \ws -> do
takeFocusX w = do
    dpy <- asks display
    wmtakef <- atom_WM_TAKE_FOCUS
    wmprot <- atom_WM_PROTOCOLS

    protocols <- io $ getWMProtocols dpy w
    when (wmtakef `elem` protocols) $ do
        io $ allocaXEvent $ \ev -> do
            setEventType ev clientMessage
            setClientMessageEvent ev w wmprot 32 wmtakef currentTime
            sendEvent dpy w False noEventMask ev

--firefox utility windows fix
getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

checkAtom name value = ask >>= \w -> liftX $ do
    a <- getAtom name
    val <- getAtom value
    mbr <- getProp a w
    case mbr of
        Just [r] -> return $ elem (fromIntegral r) [val]
        _ -> return False

checkDialog = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
checkMenu = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_MENU"
checkUtility = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY"
checkSkipTaskbar = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_STATE_SKIP_TASKBAR"

manageDialogs = checkDialog --> doFloat
manageMenus = checkMenu --> doFloat
manageUtilities = checkUtility --> doIgnore
-- manageFixes = manageDialogs <+> manageMenus <+> manageUtilities
manageFixes = manageMenus

-------------------

klog :: String -> X()
klog s = liftIO $ klogio s

klogio :: String -> IO()
klogio s = do
    h <- openFile "/tmp/xk" AppendMode
    hPutStrLn h s
    hClose h

(~=?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~=? x = fmap (isInfixOf x) q

(|=?) :: Eq a => Query [a] -> [[a]] -> Query Bool
q |=? x = fmap (flip elem x) q

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

name :: Query String
name = stringProperty "WM_NAME"

clas :: Query String
clas = stringProperty "WM_CLASS"

isNotFloating :: Query Bool
isNotFloating = ask >>= \w -> liftX $ withWindowSet $ return . not . M.member w . W.floating

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t e = do b <- p
               if b then t else e

-------------------

myMod = mod4Mask

myWorkspaces = map show [1..8] ++ ["im", "t"]

empathyRoster = (ClassName "Empathy") `And` (Role "contact_list")
empathy = (ClassName "Empathy")
psiRoster = (ClassName "psi") `And` (Resource "main")
psiOther = ClassName "psi"
vacuumRoster = (ClassName "Vacuum") `And` (Role "MainWindow")
vacuumOther = ClassName "Vacuum"
roster = psiRoster `Or` vacuumRoster
konversation = ClassName "Konversation"
skype = ClassName "Skype"
pidgin = ClassName "Pidgin"
onIM = empathy `Or` skype `Or` konversation `Or` roster `Or` psiOther `Or` vacuumOther `Or` pidgin


myLayoutHook = desktopLayoutModifiers {-$ fixFocus -} $ minimize $ workspaceDir "/home/ksobolev" $ avoidStruts $ trackFloating $
                boringWindows $ smartBorders $ windowNavigation $ toggleLayouts myTabbed $
--                chat $
                mouseResizableTile {draggerType = BordersDragger} |||
                mouseResizableTileMirrored {draggerType = BordersDragger} |||
                spiral (6/7) ||| MosaicAlt M.empty -- ||| myTabbed
                where
                chat = onWorkspace "im" chatLayout

                simpleTall = Tall 0 (1/200) (1/2)

                myTabbed   = cycleFocus $ tabbed shrinkText myTheme

                -- chatLayout = chat2
                -- chatLayout = combineTwoP (dragPane Vertical 0.1 (6/7))   chat2 simpleTall (Not empathyRoster)
                chatLayout = combineTwoP (dragPane Vertical 0.1 (6/7))   chat2 simpleTall (Not roster)
                -- chat1      = combineTwoP (dragPane Horizontal 0.1 (2/3)) chat2 simpleTall (Not konversation)
                chat2      = Grid -- False
                -- chat2      = MosaicAlt M.empty

myTheme :: Theme
myTheme = defaultTheme
    { activeColor = lightBackgroundColor
    , inactiveColor = backgroundColor
    , urgentColor = backgroundColor
    , activeBorderColor = textColor
    , inactiveTextColor = textColor
    , urgentTextColor = textColor
    , inactiveBorderColor = lightBackgroundColor
    , urgentBorderColor = myUrgentColor
    , activeTextColor = lightTextColor
    , fontName = myFont
    }

myFont = "xft:DejaVu Sans:size=10:encoding=iso10646-1"
focusColor = "#60ff45"
textColor = "#525244"
lightTextColor = "#000000"
-- lightBackgroundColor = "#FFFFD9"
lightBackgroundColor = "#EDDF61"
-- backgroundColor = "#EDDF61"
backgroundColor = "#EDEFD0"
myUrgentColor = "#ffc000"

myManageHook = composeAll [
--          propertyToQuery onMain     --> moveTo "main"
        title ~=? "IntelliJ IDEA"  --> moveTo "3"
         , (className ~=? ideaClassName) <&&> (title |=? ideaWindowsOn4)  --> (moveTo "4") <+> doSink
        -- , (className ~=? ideaClassName) <&&> (resource =? "sun-awt-X11-XDialogPeer") <&&> (title =? " ") --> doIgnore -- ignore ctrl-n window
        , propertyToQuery onIM       --> moveTo "im"

        , className =? "MPlayer"     --> doFloat
        , className =? "Gimp"        --> doFloat
        , className =? "Pandora"     --> doIgnore -- (doFloat <+> (insertPosition Below Older))
       -- , className =? "Pandora"     --> (doFloat <+> doIgnore)
        --, className =? "Pandora"     --> (doFloat)

--        , resource  =? "stalonetray" --> doIgnore
--        , className =? "Conky"       --> doIgnore
        ]
        where
                moveTo = doF . W.shift
                doSink = ask >>= doF . W.sink
                ideaClassName = "jetbrains-idea"
                ideaWindowsOn4 = ["Project", "Structure", "Run"]

--manageIdeaWindows =
--        ((className =? "jetbrains-idea") <&&> (title ~=? "win") --> doIgnore) <+>
--        ((className =? "jetbrains-idea") --> doSink)

--getEditor :: IO String
--getEditor = return "gvim"

leftScreen = 1
rightScreen = 0

shiftAndMoveTo :: Direction1D -> WSType -> X()
shiftAndMoveTo dir t = do
        wkspid <- findWorkspace getSortByIndex dir t 1
        (windows . W.shift) wkspid
        (windows . (viewOnScreen leftScreen)) wkspid
        -- (windows . W.greedyView) wkspid

shiftAndMoveToNextFree = shiftAndMoveTo Next EmptyWS

swapScreens = do
        screen <- gets (listToMaybe . W.visible . windowset)
        whenJust screen $ windows . W.greedyView . W.tag . W.workspace

myKeys x =
    [((m .|. myMod, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces x) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [ (onlyOnScreen rightScreen, 0)         -- mod + 1..0 - switch to workspace on right screen
                   , (onlyOnScreen leftScreen, controlMask) -- ctrl + mod + 1..0 - switch to workspace on left screen
                   , (W.greedyView, controlMask .|. shiftMask) -- ctrl+shift+mod + 1..0 - default greedyView
                   , (W.shift, shiftMask)]]                 -- shift + mod + 1..0 - move window to workspace

    ++
    -- mod-{e,w,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{e,w,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask x, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    [
{-        ((0, 0x1008FF5B), windows $ W.greedyView "main")
       ,((0, 0x1008FF32), windows $ W.greedyView "idea")
       ,((0, 0x1008FF43), windows $ W.greedyView "chat")
       ,((0, 0x1008FF44), windows $ W.greedyView "gimp")
       ,((0, 0x1008FF45), windows $ W.greedyView "float")
-}
       ((myMod, xK_F3),        shellPrompt myXPConfig)
       ,((modMask x, xK_Return), sendMessage ToggleLayout)
       ,((myMod, xK_Return),    windows W.swapMaster)
       ,((mod1Mask, xK_F4),     kill)         --close on alt+f4
       ,((modMask x, xK_f),     focusUrgent)
       ,((modMask x, xK_z),     withFocused $ windows . W.sink)
       ,((modMask x .|. shiftMask, xK_z),     sinkAll)
--       ,((modMask x .|. shiftMask, xK_q),     spawn "xfce4-session-logout")

--       ,((myMod, xK_Up),        sendMessage $ Go U)
--       ,((myMod, xK_Down),      sendMessage $ Go D)
--       ,((myMod, xK_Right),     sendMessage FocusNext)
--       ,((myMod, xK_Left),      sendMessage FocusPrev)

--       , ((modMask x, xK_semicolon),    focusHistoryBack)
--       , ((modMask x, xK_apostrophe),   focusHistoryForward)
--       , ((mod1Mask, xK_Tab),                   focusHistoryBack)
--       , ((mod1Mask .|. controlMask, xK_Tab),   focusHistoryForward)

--       ,((myMod, xK_Right), sendMessage $ ModifyWindowSubset (W.focusDown'::(Stack Window -> Stack Window)))
--       ,((myMod, xK_Left), sendMessage $ ModifyWindowSubset (W.focusUp'::(Stack Window -> Stack Window)))

--       ,((modMask x, xK_g),     sendMessage $ ToggleGaps)
--       ,((modMask x, xK_Up),    sendMessage $ Move U)
--       ,((modMask x, xK_Down),  sendMessage $ Move D)
--       ,((modMask x, xK_Right), sendMessage $ Move R)
--       ,((modMask x, xK_Left),  sendMessage $ Move L)

--       ,((modMask x, xK_s),                sendMessage $ SwapWindow)
--       ,((modMask x .|. shiftMask, xK_s),  sendMessage $ SwapWindowN 2)
--       ,((modMask x, xK_equal),            sendMessage $ MirrorExpand)
--       ,((modMask x, xK_minus),            sendMessage $ MirrorShrink)
       ,((modMask x, xK_equal),            sendMessage $ ExpandSlave)
       ,((modMask x, xK_minus),            sendMessage $ ShrinkSlave)
       ,((modMask x, xK_bracketleft),      sendMessage $ Shrink)
       ,((modMask x, xK_bracketright),     sendMessage $ Expand)

       ,((modMask x, xK_e),                shiftAndMoveToNextFree) -- take current window, move it to empty wksp and show it on left screen
       ,((modMask x, xK_x),                windows W.swapMaster)
       ,((modMask x, xK_s),                swapScreens)

-- esc-m  minimize, esc-r  restore
       ,((modMask x, xK_m), withFocused (\f -> sendMessage (MinimizeWin f)))
       ,((modMask x, xK_r), sendMessage RestoreNextMinimizedWin)

-- meta-alt-space: warp pointer to the center of focused window
       ,((mod1Mask .|. myMod, xK_space), warpToWindow 0.5 0.5)

       ,((myMod, xK_f),   ror browser                              browserClass)
       ,((0, 0x1008FF18), ror browser                              browserClass)
       ,((myMod, xK_e),   ror "gvim"                               (className =? "Gvim"))
       ,((myMod, xK_p),   ror "psi-plus"                                (className =? "psi"))
--       ,((myMod, xK_q),   ror "konqueror"                          (className =? "Konqueror"))
       ,((myMod, xK_k),   ror "/usr/bin/urxvt"                       (className =? "URxvt"))
--       ,((0, 0x1008FF1B), ror "/usr/bin/urxvt -e screen -S screen" (className =? "URxvt"))
       -- ,((myMod, xK_k),   ror "/usr/bin/gnome-terminal"            (className =? "Gnome-terminal"))
       -- ,((myMod, xK_k),   ror "/usr/bin/xfce4-terminal"            (className =? "Xfce4-terminal"))
       -- ,((myMod, xK_k),   ror "/usr/bin/st"                             (className =? "st-256color"))
       ,((myMod, xK_i),   ror "/home/ksobolev/idea/bin/idea.sh"         (title    ~=? "IntelliJ IDEA"))
       ,((0, 0x1008FF19), ror "/home/ksobolev/idea/bin/idea.sh"         (title    ~=? "IntelliJ IDEA"))

       -- tags
       ,((modMask x, xK_k), submap . M.fromList $
          [ ((0, xK_t),     withFocused (toggleTag "tag"))
          , ((0, xK_a),     withTaggedGlobalP "tag" shiftHere)
          ])

        -- searching
        ,((myMod, xK_slash),               submap . mySearchMap $ myPromptSearch)
        ,((myMod .|. shiftMask, xK_slash), submap . mySearchMap $ mySelectSearch)
        ,((myMod, xK_q),                   mySelectSearch mainsearch)
        ,((myMod, xK_g),                   mySelectSearch google')

        ,((myMod, xK_b),                   promptSelection browser)

       ,((0, 0x1008FF41),     namedScratchpadAction scratchpads "notes")
       ,((0, 0x1008FF30),     namedScratchpadAction scratchpads "notes")
       ,((0, 0x1008FF1D),     namedScratchpadAction scratchpads "dict")
       ,((myMod, xK_s),       namedScratchpadAction scratchpads "dict")
       ,((myMod, xK_t),       namedScratchpadAction scratchpads "htop")
       -- ,((myMod, xK_m),       namedScratchpadAction scratchpads "ncmpc")
       ,((myMod, xK_m),       namedScratchpadAction scratchpads "mutt")
       ,((myMod, xK_c),       namedScratchpadAction scratchpads "calc")
       ,((myMod, xK_space),   namedScratchpadAction scratchpads "term")
    ] where
        ror x p = (raiseNextMaybe . spawn) x (p <&&> isNotFloating)
        toggleTag s w = ifM (hasTag s w) (delTag s w) (addTag s w)



-- Perform a search, using the given method, based on a keypress
mySearchMap method = M.fromList $
        [ ((0, xK_q), method mainsearch)
        , ((0, xK_g), method google')
        , ((0, xK_w), method wikipedia')
        , ((0, xK_h), method hoogle')
        , ((shiftMask, xK_h), method hackage')
        , ((0, xK_s), method scalex)
        , ((shiftMask, xK_s), method scholar')
        , ((0, xK_m), method mathworld')
        , ((0, xK_a), method alpha')
        , ((0, xK_z), method amazon')
        , ((0, xK_p), method maps')
        , ((0, xK_t), method thesaurus')
        , ((0, xK_d), method dictionary')
        , ((0, xK_y), method youtube')
        -- , ((0, xK_b), method jira)
        , ((0, xK_c), method grepcode)
        ]

utf8SearchEngine :: SearchEngine -> SearchEngine
utf8SearchEngine (SearchEngine name site) = searchEngineF name (utf8Encode . site)

--mainsearch  = google'
mainsearch  = duckduckgo
google'     = utf8SearchEngine google
wikipedia'  = utf8SearchEngine wikipedia
hoogle'     = utf8SearchEngine hoogle
hackage'    = utf8SearchEngine hackage
scholar'    = utf8SearchEngine scholar
mathworld'  = utf8SearchEngine mathworld
alpha'      = utf8SearchEngine alpha
-- amazon'     = utf8SearchEngine amazon
amazon'     = searchEngine' "amazon" "http://amazon.com/s/ref=nb_sb_noss_1?field-keywords="
maps'       = utf8SearchEngine maps
thesaurus'  = utf8SearchEngine thesaurus
dictionary' = utf8SearchEngine dictionary
youtube'    = utf8SearchEngine youtube
jira        = searchEngine' "jira" "https://jira01.corp.linkedin.com:8443/browse/%s"
duckduckgo  = searchEngine' "duckduckgo.com" "http://duckduckgo.com?q="
grepcode    = searchEngine' "grepcode" "http://grepcode.com/search?query="
scalex      = searchEngine' "scalex" "http://scalex.org/?q="

searchEngine' name site = searchEngineF name (\s -> site ++ (utf8Encode (escape s)))

browser = "firefox"
browserClass = (className =? "Firefox")
-- browser = "chromium-browser"
-- browserClass = (className =? "Chromium-browser")

-- Prompt search: get input from the user via a prompt, then
--   run the search in firefox and automatically switch to the "wb"
--   workspace
myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->
      (search browser site s >> viewWeb)

-- Select search: do a search based on the X selection
mySelectSearch eng = selectSearch eng >> viewWeb

viewWeb = raise browserClass

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig
    { fgColor = "#101010"
    , bgColor = "#f0f0d0"
    , font = "-*-*-medium-r-*-*-14-*-*-*-*-*-iso10646-1"
    -- , font = "-xos4-terminus-medium-r-normal-*-14-*-*-*-*-*-*-*"
    }


newKeys x = M.union (M.fromList (myKeys x)) (keys defaultConfig x)

--------------- Urgency

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        --klog $ "urgency: "++(show name)++", tag: "++(show (W.findTag w ws))
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                unsafeSpawn $ "/usr/bin/notify-send '" ++ (show name ++ " requests your attention on workspace " ++ index) ++ "'"

--- status bars

-- the setup is to send data to 2 pipe files for 2 xfce applets to consume
-- workspaces and current title go to the first pipe, and layout to the second
-- since dynamicLogHook only supports single String as an output I have to
-- format it like "first string<###>second string", using <###> as a separator
-- and then later split it in the output function 'printToPipes'

pipe1 = "/home/ksobolev/pipeapplet/pipe"
pipe2 = "/home/ksobolev/pipeapplet/pipe2"

printToPipe :: String -> String -> IO ()
printToPipe p s =
        ignoreIOErrors $ do
                h <- openFile p WriteMode
                hPutStrLn h s
                hClose h
                -- klogio s

printToPipes :: String -> IO ()
printToPipes s =
        let sp = split "<###>" s
            sp1 = head sp
            sp2 = head $ tail sp
        in do
                printToPipe pipe1 sp1
                printToPipe pipe2 sp2

myPP = defaultPP {
                  -- ppLayout  = const ""
                  ppLayout  = pangoColor layoutColor
                , ppOrder = \(ws:l:ct:_) -> [ws, ct, "<###>" ++ l]
                , ppCurrent = pangoColor currentWSColor -- . wrap "[" "]"
                , ppVisible = pangoColor visibleWSColor -- . wrap "(" ")"
                , ppSort    = fmap (namedScratchpadFilterOutWorkspace.) (ppSort defaultPP)
                , ppSep     = "       "
                --, ppHidden = const ""
                , ppUrgent = pangoColor urgentColor
                , ppTitle   = wrap "<b>" "</b>" . pangoColor titleColor . shorten 120
                , ppOutput  = printToPipes.decodeString
        } where
                layoutColor = titleColor
                currentWSColor = urgentColor
                visibleWSColor = titleColor -- "blue"
                urgentColor = "red"
                titleColor = "#66A066" -- "#003366"

pangoColor :: String -> String -> String
pangoColor fg = (wrap left right).escape2
        where
                left    = "<span foreground=\"" ++ fg ++ "\">"
                right   = "</span>"
                escape2 = foldr sanitize ""
                sanitize '>' xs = "&gt;" ++ xs
                sanitize '<' xs = "&lt;" ++ xs
                sanitize '\"' xs = "&quot;" ++ xs
                sanitize '&' xs = "&amp;" ++ xs
                sanitize x xs = x:xs

--------------- Scratchpads

scratchpads = [ NS "notes"    "gvim --role notes --servername NOTES ~/notes/notes.otl" (role =? "notes") defaultRect
              , NS "htop"     "xterm -e htop"              (title     =? "htop")       defaultRect
              , NS "dict"     "goldendict"                 (className =? "Goldendict") defaultRect
              , NS "ncmpc"    "xterm -e ncmpc -C"          (title     =? "ncmpc")      smallerRect
              , NS "calc"     "xterm -e wcalc"             (title     =? "wcalc")      calcRect
              -- , NS "term"     "gnome-terminal --role term" (role      =? "term")       defaultRect
              -- , NS "term"     "xfce4-terminal --role term" (role      =? "term")       defaultRect
              -- , NS "term"     "/usr/bin/st -c term" (resource      =? "term")       defaultRect
              , NS "mutt"     "urxvt -name mutt -fn \"xft:DejaVu Sans Mono:pixelsize=16:antialias=true\" -e mutt"   (resource      =? "mutt")       defaultRect
              , NS "term"     "urxvt -name term"           (resource      =? "term")       defaultRect
              ] where
                defaultRect = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)
                smallerRect = customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3)
                calcRect    = customFloating $ W.RationalRect (3/8) (1/3) (1/4) (1/3)

--- Main
-- myConfig = gnomeConfig {
myConfig = xfceConfig {
-- myConfig dbus = xfceConfig {
             borderWidth = 1
            , modMask            = mod3Mask
            , keys               = newKeys
            , workspaces         = myWorkspaces
            -- , startupHook        = startupHook gnomeConfig >> setWMName "LG3D" -- fix for Java
            , layoutHook         = myLayoutHook

            -- , handleEventHook    = mconcat [{- (focusHistoryHook 10), -}logEventHook, perWindowKbdLayout, ewmhDesktopsEventHook]
            , handleEventHook    = mconcat [{- (focusHistoryHook 10), -} perWindowKbdLayout, ewmhDesktopsEventHook]
            --, handleEventHook    = mconcat [logEventHook, perWindowKbdLayout, ewmhDesktopsEventHook]
            --, handleEventHook    = ewmhDesktopsEventHook
            --
            --, logHook            = takeTopFocus >> ewmhDesktopsLogHook >> setWMName "LG3D"  >> dynamicLogWithPP myPP >> updatePointer (TowardsCentre 1 1)
            , logHook            = ewmhDesktopsLogHook >> setWMName "LG3D"  >> dynamicLogWithPP myPP
            -- , logHook            = ewmhDesktopsLogHook >> setWMName "LG3D"  >> dynamicLogWithPP (dbusPP dbus)
            --
            , manageHook         = myManageHook <+> (namedScratchpadManageHook scratchpads) <+> manageFixes <+> manageDocks

            --, terminal           = "gnome-terminal"
            --, terminal           = "xfce4-terminal"
            , terminal             = "urxvt"
            -- , terminal             = "/usr/bin/st"

            , normalBorderColor  = "#90dd70"
            , focusedBorderColor = "#a93131" }

--main = xmonad gnomeConfig

main = do
        config <- withWindowNavigation (xK_Up, xK_Left, xK_Down, xK_Right) $ myConfig
        xmonad $ withUrgencyHook LibNotifyUrgencyHook config
        --xmonad config

-- main = withConnection Session $ \dbus -> do
--         getWellKnownName dbus
--         config <- withWindowNavigation (xK_Up, xK_Left, xK_Down, xK_Right) $ myConfig dbus
--         xmonad config
