{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A module for building Bugsnag report payloads.
module Network.Bugsnag
  ( -- * Sending reports
    send,

    -- * Report
    Report (..),
    Event (..),
    Exception (..),
    StackFrame (..),
    Breadcrumb (..),
    Request (..),
    Thread (..),
    SeverityReason (..),
    SeverityReasonAttributes (..),
    User (..),
    App (..),
    Device (..),
    RuntimeVersions (..),
    Session (..),
    SessionEvents (..),

    -- ** PayloadVersion
    PayloadVersion,
    payloadVersion5,

    -- ** Notifier
    Notifier,
    thisNotifier,

    -- ** ExceptionType
    ExceptionType,
    cocoaExceptionType,
    androidExceptionType,
    browserjsExceptionType,
    expojsExceptionType,
    nodejsExceptionType,

    -- ** BreadcrumbType
    BreadcrumbType,
    navigationBreadcrumbType,
    requestBreadcrumbType,
    processBreadcrumbType,
    logBreadcrumbType,
    userBreadcrumbType,
    stateBreadcrumbType,
    errorBreadcrumbType,
    manualBreadcrumbType,

    -- ** Thread
    ThreadType,
    cocoaThreadType,
    androidThreadType,
    browserjsThreadType,

    -- ** Severity
    Severity,
    errorSeverity,
    warningSeverity,
    infoSeverity,

    -- ** SeverityReasonType
    SeverityReasonType,
    unhandledExceptionSeverityReasonType,
    unhandledErrorSeverityReasonType,
    logSeverityReasonType,
    signalSeverityReasonType,
    strictModeSeverityReasonType,
    unhandledPromiseRejectionSeverityReasonType,
    callbackErrorInterceptSeverityReasonType,
    errorClassSeverityReasonType,
    unhandledPanicSeverityReasonType,
    userCallbackSetSeveritySeverityReasonType,
    userSpecifiedSeveritySeverityReasonType,
    handledExceptionSeverityReasonType,
    handledErrorSeverityReasonType,
    handledPanicSeverityReasonType,
    userContextSetSeveritySeverityReasonType,
    anrErrorSeverityReasonType,
    outOfMemorySeverityReasonType,

    -- ** BinaryArch
    BinaryArch,
    x86BinaryArch,
    x86_64BinaryArch,
    arm32BinaryArch,
    arm64BinaryArch,

    -- ** CpuAbi
    CpuAbi,
    x86_64CpuAbi,
  )
where

import qualified Data.Aeson
import qualified Data.ByteString.Char8
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text.Encoding
import qualified Data.Time.Clock
import qualified Data.Time.Format
import qualified Distribution.PackageDescription.TH as CabalFile
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as Status
import Prelude

-- |
-- Send a report to Bugsnag.
send :: HTTP.Manager -> Text -> Report -> IO Bool
send manager apiKey report = do
  now <- Data.Time.Clock.getCurrentTime
  initReq <- HTTP.parseRequest "https://notify.bugsnag.com"
  let req =
        initReq
          { HTTP.method = "POST",
            HTTP.requestHeaders =
              [ ("Bugsnag-Api-Key", Data.Text.Encoding.encodeUtf8 apiKey),
                ("Content-Type", "application/json"),
                ("Bugsnag-Payload-Version", "5"),
                ("Bugsnag-Sent-At", Data.ByteString.Char8.pack (formatISO8601 now))
              ],
            HTTP.requestBody = HTTP.RequestBodyLBS (Data.Aeson.encode report)
          }
  let handleResponse = pure . Status.statusIsSuccessful . HTTP.responseStatus
  HTTP.withResponse req manager handleResponse

formatISO8601 :: Data.Time.Clock.UTCTime -> String
formatISO8601 = Data.Time.Format.formatTime Data.Time.Format.defaultTimeLocale "%FT%T%QZ"

-- | The payload of a POST request to https://notify.bugsnag.com/
data Report
  = Report
      { -- | The API Key associated with the project. Informs Bugsnag which project has generated this error.
        -- This is provided for legacy notifiers. It is preferable to use the Bugsnag-Api-Key header instead.
        report_apiKey :: Maybe Text,
        -- | The version number of the payload. This is currently 5.
        -- The Bugsnag-Payload-Version header should be included as well, for compatibility reasons.
        report_payloadVersion :: PayloadVersion,
        -- | Describes the notifier itself. These properties are used within Bugsnag to track error rates from a notifier.
        report_notifier :: Notifier,
        -- | An array of error events that Bugsnag should be notified of. A notifier can choose to group notices into an array to minimize network traffic, or can notify Bugsnag each time an event occurs.
        report_events :: [Event]
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Report where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | The version number of the payload. This is currently 5.
-- The Bugsnag-Payload-Version header should be included as well, for compatibility reasons.
newtype PayloadVersion = PayloadVersion Text
  deriving (Generic, Show)

instance Data.Aeson.ToJSON PayloadVersion where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | This API only supports payload version 5.
payloadVersion5 :: PayloadVersion
payloadVersion5 = PayloadVersion "5"

-- | Describes the notifier itself. These properties are used within Bugsnag to track error rates from a notifier.
data Notifier
  = Notifier
      { -- | The notifier name.
        notifier_name :: Text,
        -- | The notifier's current version.
        notifier_version :: Text,
        -- | The URL associated with the notifier.
        notifier_url :: Text
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Notifier where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | Information describing the notifier in this module.
thisNotifier :: Notifier
thisNotifier =
  -- The fields below are read directly out of the cabal file for this project.
  Notifier
    { notifier_name = $(CabalFile.packageVariable (CabalFile.pkgName . CabalFile.package)),
      notifier_version = $(CabalFile.packageVariable (CabalFile.pkgVersion . CabalFile.package)),
      notifier_url = $(CabalFile.packageVariable (CabalFile.packageString . CabalFile.homepage))
    }

-- | An array of error events that Bugsnag should be notified of. A notifier can choose to group notices into an array to minimize network traffic, or can notify Bugsnag each time an event occurs.
data Event
  = Event
      { -- | An array of exceptions that occurred during this event. There must be at least one entry. Most of the time there will only be one exception, but some languages support "nested" or "caused by" exceptions. In this case, exceptions should be unwrapped and added to the array one at a time. The first exception raised should be first in this array.
        event_exceptions :: [Exception],
        -- | An array of user- and system-initiated events which led up to an error, providing additional context. This list is sequential and ordered newest to oldest.
        event_breadcrumbs :: Maybe [Breadcrumb],
        -- | Details about the web request from the client that experienced the error, if relevant. To display custom request data alongside these standard fields on the Bugsnag website, the custom data should be included in the metaData object in a request object.
        event_request :: Maybe Request,
        -- | An array of background threads. This is optional but recommended for apps that rely heavily on threading. Threads should be in an order that makes sense for your application.
        event_threads :: Maybe [Thread],
        -- | A string representing what was happening in the application at the time of the error. This string could be used for grouping purposes, depending on the event. Usually this would represent the controller and action in a server based project. It could represent the screen that the user was interacting with in a client side project. For example:
        -- - On Ruby on Rails the context could be controller#action.
        -- - In Android, the context could be the top most Activity.
        -- - In iOS, the context could be the name of the top most UIViewController.
        event_context :: Maybe Text,
        -- | Bugsnag's default error grouping can be overridden by specifying a custom grouping hash.
        event_groupingHash :: Maybe Text,
        -- | Whether the error was unhandled. If true, the error was detected by the notifier because it was not handled by the application. If false, the errors was handled and reported using Bugsnag.notify.
        event_unhandled :: Maybe Bool,
        -- | The severity of the error
        event_severity :: Maybe Severity,
        -- | Information about why the severity was picked.
        event_severityReason :: Maybe SeverityReason,
        -- | Information about the user affected by the error. These fields are optional but highly recommended. To display custom user data alongside these standard fields on the Bugsnag website, the custom data should be included in the metaData object in a user object.
        event_user :: Maybe User,
        -- | Information about the app where the error occurred. These fields are optional but highly recommended. To display custom app data alongside these standard fields on the Bugsnag website, the custom data should be included in the metaData object in an app object.
        event_app :: Maybe App,
        -- | Information about the computer/device running the app. These fields are optional but highly recommended. To display custom device data alongside these standard fields on the Bugsnag website, the custom data should be included in the metaData object in a device object.
        event_device :: Maybe Device,
        -- | Details of any session information associated with the event.
        -- This can be used alongside the Bugsnag Session Tracking API to associate the event with a session so that a release's crash rate can be determined.
        event_session :: Maybe Session,
        -- | An object containing any further data you wish to attach to this error event. This should contain one or more objects, with each object being displayed in its own tab on the event details on Bugsnag.
        event_metaData :: Maybe Data.Aeson.Object
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Event where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | An exception that occurred during this event.
data Exception
  = Exception
      { -- | The class of error which occurred. This field is used to group the errors together so should not contain any contextual information that would prevent correct grouping. This would ordinarily be the Exception name when dealing with an exception.
        exception_errorClass :: Text,
        -- | The error message associated with the error. Usually this will contain some information about this specific instance of the error and is not used to group the errors.
        exception_message :: Maybe Text,
        -- | An array of stackframe objects. Each object represents one line in the exception's stacktrace. Bugsnag uses this information to help with error grouping, as well as displaying it to the user.
        exception_stacktrace :: [StackFrame],
        -- | This should be set for the following platforms so that the stacktrace can be parsed correctly:
        exception_type :: Maybe ExceptionType
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Exception where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | Each stackrame represents one line in the exception's stacktrace. Bugsnag uses this information to help with error grouping, as well as displaying it to the user.
data StackFrame
  = StackFrame
      { -- | The file that this stack frame was executing. It is recommended that you strip any unnecessary or common information from the beginning of the path.
        stackFrame_file :: Text,
        -- | The line of the file that this frame of the stack was in.
        stackFrame_lineNumber :: Int,
        -- | The column of the file that this frame of the stack was in.
        stackFrame_columnNumber :: Maybe Int,
        -- | The method that this particular stack frame is within.
        stackFrame_method :: Text,
        -- | If this stacktrace line is in the user's project code, set this to true. It is useful for developers to be able to see which lines of a stacktrace are within their own application, and which are within third party libraries. This boolean field allows Bugsnag to display this information in the stacktrace as well as use the information to help group errors better.
        stackFrame_inProject :: Maybe Bool,
        -- | The code in this file surrounding this line. This is an object containing key value pairs where each key is a line number and each value is the code from that line. You can include up to three lines on either side of the line where the error occurred. These will be displayed on the bugsnag dashboard when you expand that line.
        stackFrame_code :: Maybe (HashMap Int Text)
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON StackFrame where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | This should be set for the following platforms so that the stacktrace can be parsed correctly:
newtype ExceptionType = ExceptionType Text
  deriving (Generic, Show)

instance Data.Aeson.ToJSON ExceptionType where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | For cocoa (iOS/tvOS/macOS)
cocoaExceptionType :: ExceptionType
cocoaExceptionType = ExceptionType "cocoa"

-- | For android
androidExceptionType :: ExceptionType
androidExceptionType = ExceptionType "android"

-- | For browser-based JavaScript
browserjsExceptionType :: ExceptionType
browserjsExceptionType = ExceptionType "browserjs"

-- | For JavaScript in Expo
expojsExceptionType :: ExceptionType
expojsExceptionType = ExceptionType "expojs"

-- | For JavaScript in Node
nodejsExceptionType :: ExceptionType
nodejsExceptionType = ExceptionType "nodejs"

-- | User- and system-initiated event which led up to an error, providing additional context.
data Breadcrumb
  = Breadcrumb
      { -- | The time at which the event occurred, in [ISO 8601 format](https://tools.ietf.org/html/rfc3339#section-5.8).
        breadcrumb_timestamp :: Text,
        -- | A short summary describing the event, such as the user action taken or a new application state.
        breadcrumb_name :: Text,
        -- | A category which describes the breadcrumb, from the list of allowed values.
        breadcrumb_type :: BreadcrumbType,
        -- | Additional information about the event, as key/value pairs.
        breadcrumb_metaData :: Maybe (HashMap Text Text)
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Breadcrumb where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | A category which describes the breadcrumb, from the list of allowed values.
newtype BreadcrumbType = BreadcrumbType Text
  deriving (Generic, Show)

instance Data.Aeson.ToJSON BreadcrumbType where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | Changing screens or content being displayed, with a defined destination and optionally a previous location.
navigationBreadcrumbType :: BreadcrumbType
navigationBreadcrumbType = BreadcrumbType "navigation"

-- | Sending and receiving requests and responses.
requestBreadcrumbType :: BreadcrumbType
requestBreadcrumbType = BreadcrumbType "request"

-- | Performing an intensive task or query.
processBreadcrumbType :: BreadcrumbType
processBreadcrumbType = BreadcrumbType "process"

-- | Messages and severity sent to a logging platform.
logBreadcrumbType :: BreadcrumbType
logBreadcrumbType = BreadcrumbType "log"

-- | Actions performed by the user, like text input, button presses, or confirming/cancelling an alert dialog.
userBreadcrumbType :: BreadcrumbType
userBreadcrumbType = BreadcrumbType "user"

-- | Changing the overall state of an app, such as closing, pausing, or being moved to the background, as well as device state changes like memory or battery warnings and network connectivity changes.
stateBreadcrumbType :: BreadcrumbType
stateBreadcrumbType = BreadcrumbType "state"

-- | An error which was reported to Bugsnag encountered in the same session.
errorBreadcrumbType :: BreadcrumbType
errorBreadcrumbType = BreadcrumbType "error"

-- | User-defined, manually added breadcrumbs.
manualBreadcrumbType :: BreadcrumbType
manualBreadcrumbType = BreadcrumbType "manual"

-- | Details about the web request from the client that experienced the error, if relevant. To display custom request data alongside these standard fields on the Bugsnag website, the custom data should be included in the metaData object in a request object.
data Request
  = Request
      { -- | The IP address of the client that experienced the error.
        request_clientIp :: Maybe Text,
        -- | The headers sent with the request.
        request_headers :: Maybe (HashMap Text Text),
        -- | The HTTP method used.
        request_httpMethod :: Maybe Text,
        -- | The URL of the request.
        request_url :: Maybe Text,
        -- | The [HTTP referer](https://en.wikipedia.org/wiki/HTTP_referer)
        request_referer :: Maybe Text
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Request where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | An array of background threads. This is optional but recommended for apps that rely heavily on threading. Threads should be in an order that makes sense for your application.
data Thread
  = Thread
      { -- | The id of the thread in your application.
        thread_id :: Maybe Text,
        -- | A human readable name for the thread.
        thread_name :: Maybe Text,
        -- | If this is the thread that the error was reported from (either an unhandled error or a call to bugsnag.notify), set this to true.
        thread_errorReportingThread :: Maybe Bool,
        -- | An array of stacktrace objects. Each object represents one line in the stacktrace of the thread at the point that the error occurred.
        thread_stacktrace :: Maybe [StackFrame],
        -- | This should be set for the following platforms so that the stacktrace can be parsed correctly:
        thread_type :: Maybe ThreadType
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Thread where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

newtype ThreadType = ThreadType Text
  deriving (Generic, Show)

instance Data.Aeson.ToJSON ThreadType where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | For cocoa (iOS/tvOS/macOS)
cocoaThreadType :: ThreadType
cocoaThreadType = ThreadType "cocoa"

-- | For android
androidThreadType :: ThreadType
androidThreadType = ThreadType "android"

-- | For browser-based JavaScript
browserjsThreadType :: ThreadType
browserjsThreadType = ThreadType "browserjs"

-- | The severity of the error
newtype Severity = Severity Text
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Severity where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | The default for unhandled errors.
errorSeverity :: Severity
errorSeverity = Severity "error"

-- | The default when Bugsnag.notify is called.
warningSeverity :: Severity
warningSeverity = Severity "warning"

-- | Can be used in manual Bugsnag.notify calls.
infoSeverity :: Severity
infoSeverity = Severity "info"

-- | Information about why the severity was picked.
data SeverityReason
  = SeverityReason
      { -- | A type key that represents the reason for the assigned severity.
        severityReason_type :: SeverityReasonType,
        -- | Optional attributes to provide extra information about the severity reason.
        severityReason_attributes :: SeverityReasonAttributes
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON SeverityReason where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | A type key that represents the reason for the assigned severity.
newtype SeverityReasonType = SeverityReasonType Text
  deriving (Generic, Show)

instance Data.Aeson.ToJSON SeverityReasonType where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | Whenever an uncaught exception is discovered (generic).
unhandledExceptionSeverityReasonType :: SeverityReasonType
unhandledExceptionSeverityReasonType = SeverityReasonType "unhandledException"

-- | When an error is discovered (PHP).
unhandledErrorSeverityReasonType :: SeverityReasonType
unhandledErrorSeverityReasonType = SeverityReasonType "unhandledError"

-- | Whenever a log message is sent (generic).
logSeverityReasonType :: SeverityReasonType
logSeverityReasonType = SeverityReasonType "log"

-- | Whenever a "fatal" signal is discovered (iOS).
signalSeverityReasonType :: SeverityReasonType
signalSeverityReasonType = SeverityReasonType "signal"

-- | Whenever a strictMode issue is discovered (Android).
strictModeSeverityReasonType :: SeverityReasonType
strictModeSeverityReasonType = SeverityReasonType "strictMode"

-- | Whenever an unhandled promise rejection is discovered (JS/Node JS/React Native).
unhandledPromiseRejectionSeverityReasonType :: SeverityReasonType
unhandledPromiseRejectionSeverityReasonType = SeverityReasonType "unhandledPromiseRejection"

-- | callbackErrorIntercept (Node JS).
callbackErrorInterceptSeverityReasonType :: SeverityReasonType
callbackErrorInterceptSeverityReasonType = SeverityReasonType "callbackErrorIntercept"

-- | Whenever an exception with a particular class is automatically sent (Ruby).
errorClassSeverityReasonType :: SeverityReasonType
errorClassSeverityReasonType = SeverityReasonType "errorClass"

-- | When a panic is unhandled and crashes the app (Go).
unhandledPanicSeverityReasonType :: SeverityReasonType
unhandledPanicSeverityReasonType = SeverityReasonType "unhandledPanic"

-- | Whenever a callback changes a report's severity (generic).
userCallbackSetSeveritySeverityReasonType :: SeverityReasonType
userCallbackSetSeveritySeverityReasonType = SeverityReasonType "userCallbackSetSeverity"

-- | Whenever a severity is set through a manual notify call (generic).
userSpecifiedSeveritySeverityReasonType :: SeverityReasonType
userSpecifiedSeveritySeverityReasonType = SeverityReasonType "userSpecifiedSeverity"

-- | Whenever a handled exception is sent through (generic).
handledExceptionSeverityReasonType :: SeverityReasonType
handledExceptionSeverityReasonType = SeverityReasonType "handledException"

-- | Whenever a handled error is sent through (PHP).
handledErrorSeverityReasonType :: SeverityReasonType
handledErrorSeverityReasonType = SeverityReasonType "handledError"

-- | Whenever a panic is handled through AutoNotify or Recover (Go).
handledPanicSeverityReasonType :: SeverityReasonType
handledPanicSeverityReasonType = SeverityReasonType "handledPanic"

-- | Whenever a panic is handled through AutoNotify or Recover (Go).
userContextSetSeveritySeverityReasonType :: SeverityReasonType
userContextSetSeveritySeverityReasonType = SeverityReasonType "userContextSetSeverity"

-- | Whenever an ANR is detected (Android).
anrErrorSeverityReasonType :: SeverityReasonType
anrErrorSeverityReasonType = SeverityReasonType "anrError"

-- | When an app is terminated because it used too much memory (Cocoa).
outOfMemorySeverityReasonType :: SeverityReasonType
outOfMemorySeverityReasonType = SeverityReasonType "outOfMemory"

-- | Optional attributes to provide extra information about the severity reason.
data SeverityReasonAttributes
  = SeverityReasonAttributes
      { -- | Included for unhandledError severity reason. See [PHP Error Constants](https://www.php.net/manual/en/errorfunc.constants.php).
        severityreasonAttributes_errorType :: Maybe Text,
        -- | Included for log severity reason.
        severityreasonAttributes_level :: Maybe Text,
        -- | Included for signal severity reason. See [Signal Codes](https://en.wikipedia.org/wiki/C_signal_handling).
        severityreasonAttributes_signalType :: Maybe Text,
        -- | Included for strictMode severity reason. See [Strict Mode](https://developer.android.com/reference/android/os/StrictMode.html).
        severityreasonAttributes_violationType :: Maybe Text,
        -- | Included for errorClass severity reason. Specifies the error class that is automatically sent.
        severityreasonAttributes_errorClass :: Maybe Text
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON SeverityReasonAttributes where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | Information about the user affected by the error. These fields are optional but highly recommended. To display custom user data alongside these standard fields on the Bugsnag website, the custom data should be included in the metaData object in a user object.
data User
  = User
      { -- | A unique identifier for a user affected by this event. This could be any distinct identifier that makes sense for your application/platform.
        user_id :: Maybe Text,
        -- | The user's name, or a string you use to identify them.
        user_name :: Maybe Text,
        -- | The user's email address.
        user_email :: Maybe Text
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON User where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | Information about the app where the error occurred. These fields are optional but highly recommended. To display custom app data alongside these standard fields on the Bugsnag website, the custom data should be included in the metaData object in an app object.
data App
  = App
      { -- | A unique ID for the application.
        app_id :: Maybe Text,
        -- | The version number of the application which generated the error.
        app_version :: Maybe Text,
        -- | The [version code](https://developer.android.com/studio/publish/versioning.html) of the application (Android only)
        app_versionCode :: Maybe Int,
        -- | The [bundle version/build number](https://developer.apple.com/library/archive/technotes/tn2420/_index.html) of the application (iOS/macOS/tvOS only)
        app_bundleVersion :: Maybe Text,
        -- | A unique identifier to identify a code bundle release when using tools like CodePush (mobile only).
        app_codeBundleId :: Maybe Text,
        -- | A build ID that is required to identify a specific build when the version and version code are the same.
        app_buildUUID :: Maybe Text,
        -- | The release stage that this error occurred in, for example "development", "staging" or "production".
        app_releaseStage :: Maybe Text,
        -- | A specialized type of the application, such as the worker queue or web framework used, like "rails", "mailman", or "celery".
        app_type :: Maybe Text,
        -- | The UUIDs of the [debug symbols file](http://lldb.llvm.org/symbols.html) corresponding to this application, if any.
        app_dsymUUIDs :: Maybe [Text],
        -- | How long the app has been running for in milliseconds.
        app_duration :: Maybe Int,
        -- | How long the app has been in the foreground of the device in milliseconds.
        app_durationInForeground :: Maybe Int,
        -- | Whether or not the app was in the foreground when the error occurred.
        app_inForeground :: Maybe Bool,
        -- | The architecture of the running binary (Android only).
        app_binaryArch :: Maybe BinaryArch
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON App where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | The architecture of the running binary (Android only).
newtype BinaryArch = BinaryArch Text
  deriving (Generic, Show)

instance Data.Aeson.ToJSON BinaryArch where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | x86/i386 (32-bit).
x86BinaryArch :: BinaryArch
x86BinaryArch = BinaryArch "x86"

-- | x86 (64-bit).
x86_64BinaryArch :: BinaryArch
x86_64BinaryArch = BinaryArch "x86_64"

-- | armeabi/armeabi-v7a (32-bit).
arm32BinaryArch :: BinaryArch
arm32BinaryArch = BinaryArch "arm32"

-- | arm64-v8a (64-bit).
arm64BinaryArch :: BinaryArch
arm64BinaryArch = BinaryArch "arm64"

-- | Information about the computer/device running the app. These fields are optional but highly recommended. To display custom device data alongside these standard fields on the Bugsnag website, the custom data should be included in the metaData object in a device object.
data Device
  = Device
      { -- | The hostname of the server running your code, if applicable.
        device_hostname :: Maybe Text,
        -- | A unique identifier for the device.
        device_id :: Maybe Text,
        -- | The manufacturer of the device.
        device_manufacturer :: Maybe Text,
        -- | The model of the device.
        device_model :: Maybe Text,
        -- | The model number of the device.
        device_modelNumber :: Maybe Text,
        -- | The device's operating system name.
        device_osName :: Maybe Text,
        -- | The device's operating system version.
        device_osVersion :: Maybe Text,
        -- | The number of bytes unused in the device's RAM.
        device_freeMemory :: Maybe Int,
        -- | The number of total bytes in the device's RAM.
        device_totalMemory :: Maybe Int,
        -- | The number of unused bytes on the drive running the application.
        device_freeDisk :: Maybe Int,
        -- | If a web application, the web browser used by the device.
        device_browserName :: Maybe Text,
        -- | If a web application, the version of the browser used by the device.
        device_browserVersion :: Maybe Text,
        -- | Whether or not the device has been modified to give users root access.
        device_jailBroken :: Maybe Bool,
        -- | The orientation of the device at the time of the error.
        device_orientation :: Maybe Text,
        -- | The time at which the error occurred, in [ISO 8601 format](https://tools.ietf.org/html/rfc3339#section-5.8).
        device_time :: Maybe Text,
        -- | The ABIs supported by the device (Android only).
        device_cpuAbi :: Maybe [CpuAbi],
        -- | The versions of the relevant runtimes, languages and/or frameworks for the platform.
        device_runtimeVersions :: Maybe RuntimeVersions
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Device where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | The ABIs supported by the device (Android only).
newtype CpuAbi = CpuAbi Text
  deriving (Generic, Show)

instance Data.Aeson.ToJSON CpuAbi where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- |
x86_64CpuAbi :: CpuAbi
x86_64CpuAbi = CpuAbi "x86_64"

-- | The versions of the relevant runtimes, languages and/or frameworks for the platform.
data RuntimeVersions
  = RuntimeVersions
      { -- | The Android API level (Android only).
        runtimeVersions_androidApi :: Maybe Text,
        -- | Bottle framework version (Python only).
        runtimeVersions_bottle :: Maybe Text,
        -- | Celery task queue version (Python only).
        runtimeVersions_celery :: Maybe Text,
        -- | Clang compiler version (iOS/tvOS/macOS only).
        runtimeVersions_clangVersion :: Maybe Text,
        -- | Cocos2d-x framework version (Cocos2d-x only).
        runtimeVersions_cocos2dx :: Maybe Text,
        -- | Delayed Job framework version (Ruby only).
        runtimeVersions_delayedJob :: Maybe Text,
        -- | Django framework version (Python only).
        runtimeVersions_django :: Maybe Text,
        -- | Description of the framework (.NET only).
        runtimeVersions_dotnet :: Maybe Text,
        -- | .NET API compatibility level (Unity only).
        runtimeVersions_dotnetApiCompatibility :: Maybe Text,
        -- | Version of .NET Common Language Runtime (.NET only).
        runtimeVersions_dotnetClr :: Maybe Text,
        -- | .NET scripting runtime version (Unity only).
        runtimeVersions_dotnetScriptingRuntime :: Maybe Text,
        -- | EventMachine library version (Ruby only).
        runtimeVersions_eventMachine :: Maybe Text,
        -- | Expo app version (Expo only).
        runtimeVersions_expoApp :: Maybe Text,
        -- | Expo SDK version (Expo only).
        runtimeVersions_expoSdk :: Maybe Text,
        -- | Flask framework version (Python only).
        runtimeVersions_flask :: Maybe Text,
        -- | Gin framework version (Go only).
        runtimeVersions_gin :: Maybe Text,
        -- | Go language version (Go only).
        runtimeVersions_go :: Maybe Text,
        -- | Java platform implementation type (Java only).
        runtimeVersions_javaType :: Maybe Text,
        -- | Java platform implementation type (Java only).
        runtimeVersions_javaVersion :: Maybe Text,
        -- | Version of JRuby (Ruby only).
        runtimeVersions_jruby :: Maybe Text,
        -- | Laravel framework version (PHP only).
        runtimeVersions_laravel :: Maybe Text,
        -- | Lumen framework version (PHP only).
        runtimeVersions_lumen :: Maybe Text,
        -- | Magento platform version (PHP only).
        runtimeVersions_magento :: Maybe Text,
        -- | Mailman framework version (Ruby only).
        runtimeVersions_mailman :: Maybe Text,
        -- | Martini framework version (Go only).
        runtimeVersions_martini :: Maybe Text,
        -- | Negroni framework version (Go only).
        runtimeVersions_negroni :: Maybe Text,
        -- | Node.js version (Javascript only).
        runtimeVersions_node :: Maybe Text,
        -- | Build number of the OS (iOS/tvOS/macOS only).
        runtimeVersions_osBuild :: Maybe Text,
        -- | Version of PHP (PHP only).
        runtimeVersions_php :: Maybe Text,
        -- | Version of Python (Python only).
        runtimeVersions_python :: Maybe Text,
        -- | Que job queue version (Ruby only).
        runtimeVersions_que :: Maybe Text,
        -- | Rack webserver version (Ruby only).
        runtimeVersions_rack :: Maybe Text,
        -- | Ruby on Rails version (Ruby only).
        runtimeVersions_rails :: Maybe Text,
        -- | Rake tool version (Ruby only).
        runtimeVersions_rake :: Maybe Text,
        -- | Version of React Native (React Native/Expo only).
        runtimeVersions_reactNative :: Maybe Text,
        -- | Javascript engine type (React Native/Expo only).
        runtimeVersions_reactNativeJsEngine :: Maybe Text,
        -- | Resque library version (Ruby only).
        runtimeVersions_resque :: Maybe Text,
        -- | Revel framework version (Go only).
        runtimeVersions_revel :: Maybe Text,
        -- | Version of Ruby (Ruby only).
        runtimeVersions_ruby :: Maybe Text,
        -- | Shoryoken framework version (Ruby only).
        runtimeVersions_shoryoken :: Maybe Text,
        -- | Sidekiq scheduler version (Ruby only).
        runtimeVersions_sidekiq :: Maybe Text,
        -- | Silex framework version (PHP only).
        runtimeVersions_silex :: Maybe Text,
        -- | Sinatra DSL version (Ruby only).
        runtimeVersions_sinatra :: Maybe Text,
        -- | Spring Boot framework version (Java only).
        runtimeVersions_springBoot :: Maybe Text,
        -- | Spring framework version (Java only).
        runtimeVersions_springFramework :: Maybe Text,
        -- | Swift language version (iOS/tvOS/macOS only).
        runtimeVersions_swift :: Maybe Text,
        -- | Symfony framework version (PHP only).
        runtimeVersions_symfony :: Maybe Text,
        -- | Tornado framework version (Python only).
        runtimeVersions_tornado :: Maybe Text,
        -- | Version of Unity.
        runtimeVersions_unity :: Maybe Text,
        -- | The Unity scripting backend - Mono or IL2CPP (Unity only).
        runtimeVersions_unityScriptingBackend :: Maybe Text,
        -- | Wordpress version (PHP only).
        runtimeVersions_wordpress :: Maybe Text
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON RuntimeVersions where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | Details of any session information associated with the event.
-- This can be used alongside the Bugsnag Session Tracking API to associate the event with a session so that a release's crash rate can be determined.
data Session
  = Session
      { -- | The unique identifier of the session.
        session_id :: Text,
        -- | The time (in [ISO 8601 format](https://tools.ietf.org/html/rfc3339#section-5.8)) at which the session started.
        session_startedAt :: Text,
        -- | Details of the number of handled and unhandled events that have occurred so far in this session.
        session_events :: SessionEvents
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON Session where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

-- | Details of the number of handled and unhandled events that have occurred so far in this session.
data SessionEvents
  = SessionEvents
      { -- | Details of the number of handled and unhandled events that have occurred so far in this session.
        sessionEvents_handled :: Int,
        -- | The number of unhandled events that have occurred in this session (including this event)
        sessionEvents_unhandled :: Int
      }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON SessionEvents where

  toJSON = Data.Aeson.genericToJSON aesonOptions

  toEncoding = Data.Aeson.genericToEncoding aesonOptions

aesonOptions :: Data.Aeson.Options
aesonOptions =
  Data.Aeson.defaultOptions
    { Data.Aeson.fieldLabelModifier = drop 1 . dropWhile (/= '_'),
      Data.Aeson.omitNothingFields = True
    }
