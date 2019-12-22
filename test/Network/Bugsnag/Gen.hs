module Network.Bugsnag.Gen (report) where

import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.Bugsnag as Bugsnag

text :: Gen Text
text = Gen.text (Range.linear 0 3) (Gen.enum 'a' 'Z')

int :: Gen Int
int = Gen.int (Range.linear (-5) 5)

list :: Gen a -> Gen [a]
list = Gen.list (Range.linear 0 3)

hashMap :: (Hashable k, Eq k) => Gen k -> Gen v -> Gen (HashMap.HashMap k v)
hashMap key val = HashMap.fromList <$> list ((,) <$> key <*> val)

report :: Gen Bugsnag.Report
report =
  Bugsnag.Report
    <$> Gen.maybe apiKey
    <*> pure Bugsnag.payloadVersion5
    <*> pure Bugsnag.thisNotifier
    <*> list event

apiKey :: Gen Bugsnag.ApiKey
apiKey = Bugsnag.apiKey <$> text

event :: Gen Bugsnag.Event
event =
  Bugsnag.Event
    <$> list exception
    <*> Gen.maybe (list breadcrumb)
    <*> Gen.maybe request
    <*> Gen.maybe (list thread)
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe severity
    <*> Gen.maybe severityReason
    <*> Gen.maybe user
    <*> Gen.maybe app
    <*> Gen.maybe device
    <*> Gen.maybe session
    <*> Gen.maybe (Gen.constant mempty)

exception :: Gen Bugsnag.Exception
exception =
  Bugsnag.Exception
    <$> text
    <*> Gen.maybe text
    <*> list stackFrame
    <*> Gen.maybe exceptionType

stackFrame :: Gen Bugsnag.StackFrame
stackFrame =
  Bugsnag.StackFrame
    <$> text
    <*> int
    <*> Gen.maybe int
    <*> text
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe (hashMap int text)

exceptionType :: Gen Bugsnag.ExceptionType
exceptionType =
  Gen.element
    [ Bugsnag.cocoaExceptionType,
      Bugsnag.androidExceptionType,
      Bugsnag.browserjsExceptionType,
      Bugsnag.expojsExceptionType,
      Bugsnag.nodejsExceptionType
    ]

breadcrumb :: Gen Bugsnag.Breadcrumb
breadcrumb =
  Bugsnag.Breadcrumb
    <$> text
    <*> text
    <*> breadcrumbType
    <*> Gen.maybe (hashMap text text)

breadcrumbType :: Gen Bugsnag.BreadcrumbType
breadcrumbType =
  Gen.element
    [ Bugsnag.navigationBreadcrumbType,
      Bugsnag.requestBreadcrumbType,
      Bugsnag.processBreadcrumbType,
      Bugsnag.logBreadcrumbType,
      Bugsnag.userBreadcrumbType,
      Bugsnag.stateBreadcrumbType,
      Bugsnag.errorBreadcrumbType,
      Bugsnag.manualBreadcrumbType
    ]

request :: Gen Bugsnag.Request
request =
  Bugsnag.Request
    <$> Gen.maybe text
    <*> Gen.maybe (hashMap text text)
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text

thread :: Gen Bugsnag.Thread
thread =
  Bugsnag.Thread
    <$> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe (list stackFrame)
    <*> Gen.maybe threadType

threadType :: Gen Bugsnag.ThreadType
threadType =
  Gen.element
    [ Bugsnag.cocoaThreadType,
      Bugsnag.androidThreadType,
      Bugsnag.browserjsThreadType
    ]

severity :: Gen Bugsnag.Severity
severity =
  Gen.element
    [ Bugsnag.errorSeverity,
      Bugsnag.warningSeverity,
      Bugsnag.infoSeverity
    ]

severityReason :: Gen Bugsnag.SeverityReason
severityReason =
  Bugsnag.SeverityReason
    <$> severityReasonType
    <*> severityReasonAttributes

severityReasonType :: Gen Bugsnag.SeverityReasonType
severityReasonType =
  Gen.element
    [ Bugsnag.unhandledExceptionSeverityReasonType,
      Bugsnag.unhandledErrorSeverityReasonType,
      Bugsnag.logSeverityReasonType,
      Bugsnag.signalSeverityReasonType,
      Bugsnag.strictModeSeverityReasonType,
      Bugsnag.unhandledPromiseRejectionSeverityReasonType,
      Bugsnag.callbackErrorInterceptSeverityReasonType,
      Bugsnag.errorClassSeverityReasonType,
      Bugsnag.unhandledPanicSeverityReasonType,
      Bugsnag.userCallbackSetSeveritySeverityReasonType,
      Bugsnag.userSpecifiedSeveritySeverityReasonType,
      Bugsnag.handledExceptionSeverityReasonType,
      Bugsnag.handledErrorSeverityReasonType,
      Bugsnag.handledPanicSeverityReasonType,
      Bugsnag.userContextSetSeveritySeverityReasonType,
      Bugsnag.anrErrorSeverityReasonType,
      Bugsnag.outOfMemorySeverityReasonType
    ]

severityReasonAttributes :: Gen Bugsnag.SeverityReasonAttributes
severityReasonAttributes =
  Bugsnag.SeverityReasonAttributes
    <$> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text

user :: Gen Bugsnag.User
user =
  Bugsnag.User
    <$> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text

app :: Gen Bugsnag.App
app =
  Bugsnag.App
    <$> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe int
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe (list text)
    <*> Gen.maybe int
    <*> Gen.maybe int
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe binaryArch

binaryArch :: Gen Bugsnag.BinaryArch
binaryArch =
  Gen.element
    [ Bugsnag.x86BinaryArch,
      Bugsnag.x86_64BinaryArch,
      Bugsnag.arm32BinaryArch,
      Bugsnag.arm64BinaryArch
    ]

device :: Gen Bugsnag.Device
device =
  Bugsnag.Device
    <$> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe int
    <*> Gen.maybe int
    <*> Gen.maybe int
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe (list cpuAbi)
    <*> Gen.maybe runtimeVersions

cpuAbi :: Gen Bugsnag.CpuAbi
cpuAbi = Gen.constant Bugsnag.x86_64CpuAbi

runtimeVersions :: Gen Bugsnag.RuntimeVersions
runtimeVersions =
  Bugsnag.RuntimeVersions
    <$> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text

session :: Gen Bugsnag.Session
session =
  Bugsnag.Session
    <$> text
    <*> text
    <*> sessionEvents

sessionEvents :: Gen Bugsnag.SessionEvents
sessionEvents =
  Bugsnag.SessionEvents
    <$> int
    <*> int
