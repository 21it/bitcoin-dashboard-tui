{-# LANGUAGE OverloadedStrings #-}

module BitcoinDashboardTui.Wai.Chat
  ( app,
  )
where

import Concur.Core.Types (Widget)
import Concur.Replica hiding (id, step)
import Concur.Replica.DOM (button, input, li, text, ul)
import Concur.Replica.DOM.Events (BaseEvent (target), onClick, onInput, targetValue)
import Concur.Replica.DOM.Props (value)
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import BitcoinDashboardTui.Import hiding (div)
import qualified BitcoinDashboardTui.Wai.Static as Static
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Replica as R
import Network.WebSockets (defaultConnectionOptions)

-- A simple chat app demonstrating communication between clients using STM.
-- The widget is waiting for either the user to send a message or
-- another client to alert a new message. When a client sends a message
-- the chatWidget will update the messageHistory and then alert the other
-- clients by writing to messageAlert.

mainWidget :: Env -> Widget HTML ()
mainWidget env = do
  bc <- liftIO . atomically . dupTChan $ envMsgAlert env
  div
    [className "ui container compact padded"]
    [void $ chatWidget env bc $ Message mempty]

chatWidget :: Env -> TChan () -> Message -> Widget HTML ()
chatWidget env bc msg = do
  messageHistory <- liftIO . readTVarIO $ msgsTVar
  chatAction <- messageEditor bc msg <|> messagesList messageHistory
  case chatAction of
    ChatTyping x -> chatWidget env bc x
    ChatRefresh x -> chatWidget env bc x
    ChatKeyDown _ -> chatWidget env bc msg
    ChatSend x -> sendMsg x
  where
    msgsTVar = envMsgHistory env
    messageAlert = envMsgAlert env
    sendMsg x = do
      let x0 = strip $ coerce x
      if x0 == mempty
        then chatWidget env bc x
        else do
          liftIO . atomically $ do
            -- Add this message to the messageHistory
            modifyTVar' msgsTVar (x :)
            -- Notify all clients about the updates
            writeTChan messageAlert ()
          chatWidget env bc $ Message mempty

messageEditor :: TChan () -> Message -> Widget HTML ChatEvent
messageEditor bc typing = do
  event <- editor <|> updater
  case event of
    ChatTyping x -> messageEditor bc x
    ChatKeyDown "Enter" -> pure $ ChatSend typing
    ChatKeyDown _ -> messageEditor bc typing
    ChatSend _ -> pure event
    ChatRefresh _ -> pure event
  where
    updater =
      ChatRefresh typing <$ (liftIO . atomically . readTChan $ bc)
    editor =
      div
        [className "text-align-center"]
        [ div
            [className "form-inline margin-bottom"]
            [ div
                [ classList [("form-group", True), ("has-error", False)]
                ]
                [ div
                    [className "input-group"]
                    [ input
                        [ className "form-control",
                          autofocus True,
                          value $ coerce typing,
                          type_ "text",
                          ChatTyping . Message . targetValue . target <$> onInput,
                          ChatKeyDown . kbdKey <$> onKeyDown
                        ],
                      span
                        [className "input-group-btn"]
                        [ button
                            [ className "btn btn-primary",
                              ChatSend typing <$ onClick
                            ]
                            [text "Send"]
                        ]
                    ]
                ]
            ]
        ]

messagesList :: [Message] -> Widget HTML ChatEvent
messagesList xs = ul [className "list-group"] $ messageItem <$> xs

messageItem :: Message -> Widget HTML ChatEvent
messageItem x = li [className "list-group-item"] [text $ coerce x]

app :: Env -> Wai.Application
app env =
  R.app
    (defaultIndex "Chat" Static.headerHTML)
    defaultConnectionOptions
    Static.middleware
    (step <$> const (mainWidget env))
    stepWidget
