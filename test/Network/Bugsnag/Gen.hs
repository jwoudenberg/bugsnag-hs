{-# LANGUAGE NamedFieldPuns #-}

module Network.Bugsnag.Gen
  ( report,
  )
where

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
  ( \report_apiKey
     report_payloadVersion
     report_notifier
     report_events ->
        Bugsnag.defaultReport
          { Bugsnag.report_apiKey,
            Bugsnag.report_payloadVersion,
            Bugsnag.report_notifier,
            Bugsnag.report_events
          }
  )
    <$> Gen.maybe apiKey
    <*> pure Bugsnag.payloadVersion5
    <*> pure Bugsnag.thisNotifier
    <*> list event

apiKey :: Gen Bugsnag.ApiKey
apiKey = Bugsnag.apiKey <$> text

event :: Gen Bugsnag.Event
event =
  ( \event_exceptions
     event_breadcrumbs
     event_request
     event_threads
     event_context
     event_groupingHash
     event_unhandled
     event_severity
     event_severityReason
     event_user
     event_app
     event_device
     event_session
     event_metaData ->
        Bugsnag.defaultEvent
          { Bugsnag.event_exceptions,
            Bugsnag.event_breadcrumbs,
            Bugsnag.event_request,
            Bugsnag.event_threads,
            Bugsnag.event_context,
            Bugsnag.event_groupingHash,
            Bugsnag.event_unhandled,
            Bugsnag.event_severity,
            Bugsnag.event_severityReason,
            Bugsnag.event_user,
            Bugsnag.event_app,
            Bugsnag.event_device,
            Bugsnag.event_session,
            Bugsnag.event_metaData
          }
  )
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
  ( \exception_errorClass
     exception_message
     exception_stacktrace
     exception_type ->
        Bugsnag.defaultException
          { Bugsnag.exception_errorClass,
            Bugsnag.exception_message,
            Bugsnag.exception_stacktrace,
            Bugsnag.exception_type
          }
  )
    <$> text
    <*> Gen.maybe text
    <*> list stackFrame
    <*> Gen.maybe exceptionType

stackFrame :: Gen Bugsnag.StackFrame
stackFrame =
  ( \stackFrame_file
     stackFrame_lineNumber
     stackFrame_columnNumber
     stackFrame_method
     stackFrame_inProject
     stackFrame_code ->
        Bugsnag.defaultStackFrame
          { Bugsnag.stackFrame_file,
            Bugsnag.stackFrame_lineNumber,
            Bugsnag.stackFrame_columnNumber,
            Bugsnag.stackFrame_method,
            Bugsnag.stackFrame_inProject,
            Bugsnag.stackFrame_code
          }
  )
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
  ( \breadcrumb_timestamp
     breadcrumb_name
     breadcrumb_type
     breadcrumb_metaData ->
        Bugsnag.defaultBreadcrumb
          { Bugsnag.breadcrumb_timestamp,
            Bugsnag.breadcrumb_name,
            Bugsnag.breadcrumb_type,
            Bugsnag.breadcrumb_metaData
          }
  )
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
  ( \request_clientIp
     request_headers
     request_httpMethod
     request_url
     request_referer ->
        Bugsnag.defaultRequest
          { Bugsnag.request_clientIp,
            Bugsnag.request_headers,
            Bugsnag.request_httpMethod,
            Bugsnag.request_url,
            Bugsnag.request_referer
          }
  )
    <$> Gen.maybe text
    <*> Gen.maybe (hashMap text text)
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text

thread :: Gen Bugsnag.Thread
thread =
  ( \thread_id
     thread_name
     thread_errorReportingThread
     thread_stacktrace
     thread_type ->
        Bugsnag.defaultThread
          { Bugsnag.thread_id,
            Bugsnag.thread_name,
            Bugsnag.thread_errorReportingThread,
            Bugsnag.thread_stacktrace,
            Bugsnag.thread_type
          }
  )
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
  ( \severityReason_type
     severityReason_attributes ->
        Bugsnag.defaultSeverityReason
          { Bugsnag.severityReason_type,
            Bugsnag.severityReason_attributes
          }
  )
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
  ( \severityReasonAttributes_errorType
     severityReasonAttributes_level
     severityReasonAttributes_signalType
     severityReasonAttributes_violationType
     severityReasonAttributes_errorClass ->
        Bugsnag.defaultSeverityReasonAttributes
          { Bugsnag.severityReasonAttributes_errorType,
            Bugsnag.severityReasonAttributes_level,
            Bugsnag.severityReasonAttributes_signalType,
            Bugsnag.severityReasonAttributes_violationType,
            Bugsnag.severityReasonAttributes_errorClass
          }
  )
    <$> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text

user :: Gen Bugsnag.User
user =
  ( \user_id
     user_name
     user_email ->
        Bugsnag.defaultUser
          { Bugsnag.user_id,
            Bugsnag.user_name,
            Bugsnag.user_email
          }
  )
    <$> Gen.maybe text
    <*> Gen.maybe text
    <*> Gen.maybe text

app :: Gen Bugsnag.App
app =
  ( \app_id
     app_version
     app_versionCode
     app_bundleVersion
     app_codeBundleId
     app_buildUUID
     app_releaseStage
     app_type
     app_dsymUUIDs
     app_duration
     app_durationInForeground
     app_inForeground
     app_binaryArch ->
        Bugsnag.defaultApp
          { Bugsnag.app_id,
            Bugsnag.app_version,
            Bugsnag.app_versionCode,
            Bugsnag.app_bundleVersion,
            Bugsnag.app_codeBundleId,
            Bugsnag.app_buildUUID,
            Bugsnag.app_releaseStage,
            Bugsnag.app_type,
            Bugsnag.app_dsymUUIDs,
            Bugsnag.app_duration,
            Bugsnag.app_durationInForeground,
            Bugsnag.app_inForeground,
            Bugsnag.app_binaryArch
          }
  )
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
  ( \device_hostname
     device_id
     device_manufacturer
     device_model
     device_modelNumber
     device_osName
     device_osVersion
     device_freeMemory
     device_totalMemory
     device_freeDisk
     device_browserName
     device_browserVersion
     device_jailBroken
     device_orientation
     device_time
     device_cpuAbi
     device_runtimeVersions ->
        Bugsnag.defaultDevice
          { Bugsnag.device_hostname,
            Bugsnag.device_id,
            Bugsnag.device_manufacturer,
            Bugsnag.device_model,
            Bugsnag.device_modelNumber,
            Bugsnag.device_osName,
            Bugsnag.device_osVersion,
            Bugsnag.device_freeMemory,
            Bugsnag.device_totalMemory,
            Bugsnag.device_freeDisk,
            Bugsnag.device_browserName,
            Bugsnag.device_browserVersion,
            Bugsnag.device_jailBroken,
            Bugsnag.device_orientation,
            Bugsnag.device_time,
            Bugsnag.device_cpuAbi,
            Bugsnag.device_runtimeVersions
          }
  )
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
  ( \runtimeVersions_androidApi
     runtimeVersions_bottle
     runtimeVersions_celery
     runtimeVersions_clangVersion
     runtimeVersions_cocos2dx
     runtimeVersions_delayedJob
     runtimeVersions_django
     runtimeVersions_dotnet
     runtimeVersions_dotnetApiCompatibility
     runtimeVersions_dotnetClr
     runtimeVersions_dotnetScriptingRuntime
     runtimeVersions_eventMachine
     runtimeVersions_expoApp
     runtimeVersions_expoSdk
     runtimeVersions_flask
     runtimeVersions_gin
     runtimeVersions_go
     runtimeVersions_javaType
     runtimeVersions_javaVersion
     runtimeVersions_jruby
     runtimeVersions_laravel
     runtimeVersions_lumen
     runtimeVersions_magento
     runtimeVersions_mailman
     runtimeVersions_martini
     runtimeVersions_negroni
     runtimeVersions_node
     runtimeVersions_osBuild
     runtimeVersions_php
     runtimeVersions_python
     runtimeVersions_que
     runtimeVersions_rack
     runtimeVersions_rails
     runtimeVersions_rake
     runtimeVersions_reactNative
     runtimeVersions_reactNativeJsEngine
     runtimeVersions_resque
     runtimeVersions_revel
     runtimeVersions_ruby
     runtimeVersions_shoryoken
     runtimeVersions_sidekiq
     runtimeVersions_silex
     runtimeVersions_sinatra
     runtimeVersions_springBoot
     runtimeVersions_springFramework
     runtimeVersions_swift
     runtimeVersions_symfony
     runtimeVersions_tornado
     runtimeVersions_unity
     runtimeVersions_unityScriptingBackend
     runtimeVersions_wordpress ->
        Bugsnag.defaultRuntimeVersions
          { Bugsnag.runtimeVersions_androidApi,
            Bugsnag.runtimeVersions_bottle,
            Bugsnag.runtimeVersions_celery,
            Bugsnag.runtimeVersions_clangVersion,
            Bugsnag.runtimeVersions_cocos2dx,
            Bugsnag.runtimeVersions_delayedJob,
            Bugsnag.runtimeVersions_django,
            Bugsnag.runtimeVersions_dotnet,
            Bugsnag.runtimeVersions_dotnetApiCompatibility,
            Bugsnag.runtimeVersions_dotnetClr,
            Bugsnag.runtimeVersions_dotnetScriptingRuntime,
            Bugsnag.runtimeVersions_eventMachine,
            Bugsnag.runtimeVersions_expoApp,
            Bugsnag.runtimeVersions_expoSdk,
            Bugsnag.runtimeVersions_flask,
            Bugsnag.runtimeVersions_gin,
            Bugsnag.runtimeVersions_go,
            Bugsnag.runtimeVersions_javaType,
            Bugsnag.runtimeVersions_javaVersion,
            Bugsnag.runtimeVersions_jruby,
            Bugsnag.runtimeVersions_laravel,
            Bugsnag.runtimeVersions_lumen,
            Bugsnag.runtimeVersions_magento,
            Bugsnag.runtimeVersions_mailman,
            Bugsnag.runtimeVersions_martini,
            Bugsnag.runtimeVersions_negroni,
            Bugsnag.runtimeVersions_node,
            Bugsnag.runtimeVersions_osBuild,
            Bugsnag.runtimeVersions_php,
            Bugsnag.runtimeVersions_python,
            Bugsnag.runtimeVersions_que,
            Bugsnag.runtimeVersions_rack,
            Bugsnag.runtimeVersions_rails,
            Bugsnag.runtimeVersions_rake,
            Bugsnag.runtimeVersions_reactNative,
            Bugsnag.runtimeVersions_reactNativeJsEngine,
            Bugsnag.runtimeVersions_resque,
            Bugsnag.runtimeVersions_revel,
            Bugsnag.runtimeVersions_ruby,
            Bugsnag.runtimeVersions_shoryoken,
            Bugsnag.runtimeVersions_sidekiq,
            Bugsnag.runtimeVersions_silex,
            Bugsnag.runtimeVersions_sinatra,
            Bugsnag.runtimeVersions_springBoot,
            Bugsnag.runtimeVersions_springFramework,
            Bugsnag.runtimeVersions_swift,
            Bugsnag.runtimeVersions_symfony,
            Bugsnag.runtimeVersions_tornado,
            Bugsnag.runtimeVersions_unity,
            Bugsnag.runtimeVersions_unityScriptingBackend,
            Bugsnag.runtimeVersions_wordpress
          }
  )
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
  ( \session_id
     session_startedAt
     session_events ->
        Bugsnag.defaultSession
          { Bugsnag.session_id,
            Bugsnag.session_startedAt,
            Bugsnag.session_events
          }
  )
    <$> text
    <*> text
    <*> sessionEvents

sessionEvents :: Gen Bugsnag.SessionEvents
sessionEvents =
  ( \sessionEvents_handled
     sessionEvents_unhandled ->
        Bugsnag.defaultSessionEvents
          { Bugsnag.sessionEvents_handled,
            Bugsnag.sessionEvents_unhandled
          }
  )
    <$> int
    <*> int
