{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import SimpleBilly.Model
import SimpleBilly.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary Absence where
  arbitrary = sized genAbsence

genAbsence :: Int -> Gen Absence
genAbsence n =
  Absence
    <$> arbitraryReducedMaybe n -- absenceAbsenceType :: Maybe AbsenceType
    <*> arbitraryReducedMaybe n -- absenceApprovedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- absenceApprovedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- absenceDeletedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- absenceEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- absenceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- absenceStatus :: Maybe AbsenceStatus
    <*> arbitraryReducedMaybe n -- absenceTenantId :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceUpdatedAt :: Maybe DateTime
  
instance Arbitrary AbsenceCreate where
  arbitrary = sized genAbsenceCreate

genAbsenceCreate :: Int -> Gen AbsenceCreate
genAbsenceCreate n =
  AbsenceCreate
    <$> arbitraryReducedMaybe n -- absenceCreateAbsenceType :: Maybe AbsenceType
    <*> arbitraryReducedMaybe n -- absenceCreateApprovedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- absenceCreateApprovedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceCreateEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceCreateEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- absenceCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceCreateStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- absenceCreateStatus :: Maybe AbsenceStatus
  
instance Arbitrary AbsenceUpdate where
  arbitrary = sized genAbsenceUpdate

genAbsenceUpdate :: Int -> Gen AbsenceUpdate
genAbsenceUpdate n =
  AbsenceUpdate
    <$> arbitraryReducedMaybe n -- absenceUpdateAbsenceType :: Maybe AbsenceType
    <*> arbitraryReducedMaybe n -- absenceUpdateApprovedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- absenceUpdateApprovedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceUpdateEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceUpdateEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- absenceUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- absenceUpdateStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- absenceUpdateStatus :: Maybe AbsenceStatus
  
instance Arbitrary AcceptInviteRequest where
  arbitrary = sized genAcceptInviteRequest

genAcceptInviteRequest :: Int -> Gen AcceptInviteRequest
genAcceptInviteRequest n =
  AcceptInviteRequest
    <$> arbitrary -- acceptInviteRequestFirstName :: Text
    <*> arbitrary -- acceptInviteRequestLastName :: Text
    <*> arbitrary -- acceptInviteRequestPassword :: Text
    <*> arbitrary -- acceptInviteRequestPrivacyAccepted :: Bool
    <*> arbitrary -- acceptInviteRequestToken :: Text
  
instance Arbitrary AccountOverview where
  arbitrary = sized genAccountOverview

genAccountOverview :: Int -> Gen AccountOverview
genAccountOverview n =
  AccountOverview
    <$> arbitrary -- accountOverviewAccount :: Text
    <*> arbitrary -- accountOverviewAccountName :: Text
    <*> arbitrary -- accountOverviewBalance :: Text
    <*> arbitrary -- accountOverviewCreditTotal :: Text
    <*> arbitrary -- accountOverviewDebitTotal :: Text
  
instance Arbitrary Activity where
  arbitrary = sized genActivity

genActivity :: Int -> Gen Activity
genActivity n =
  Activity
    <$> arbitraryReduced n -- activityActivityType :: ActivityType
    <*> arbitraryReducedMaybe n -- activityAssignedTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- activityReminderDate :: Maybe Date
    <*> arbitraryReduced n -- activityStatus :: ActivityStatus
    <*> arbitrary -- activitySubject :: Text
  
instance Arbitrary ActivityCreate where
  arbitrary = sized genActivityCreate

genActivityCreate :: Int -> Gen ActivityCreate
genActivityCreate n =
  ActivityCreate
    <$> arbitraryReduced n -- activityCreateActivityType :: ActivityType
    <*> arbitraryReducedMaybe n -- activityCreateAssignedTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityCreateContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityCreateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityCreateDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- activityCreateReminderDate :: Maybe Date
    <*> arbitraryReduced n -- activityCreateStatus :: ActivityStatus
    <*> arbitrary -- activityCreateSubject :: Text
  
instance Arbitrary ActivityStatusUpdate where
  arbitrary = sized genActivityStatusUpdate

genActivityStatusUpdate :: Int -> Gen ActivityStatusUpdate
genActivityStatusUpdate n =
  ActivityStatusUpdate
    <$> arbitrary -- activityStatusUpdateStatus :: Text
  
instance Arbitrary ActivityUpdate where
  arbitrary = sized genActivityUpdate

genActivityUpdate :: Int -> Gen ActivityUpdate
genActivityUpdate n =
  ActivityUpdate
    <$> arbitraryReducedMaybe n -- activityUpdateActivityType :: Maybe ActivityType
    <*> arbitraryReducedMaybe n -- activityUpdateAssignedTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityUpdateContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityUpdateDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- activityUpdateReminderDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- activityUpdateStatus :: Maybe ActivityStatus
    <*> arbitraryReducedMaybe n -- activityUpdateSubject :: Maybe Text
  
instance Arbitrary Address where
  arbitrary = sized genAddress

genAddress :: Int -> Gen Address
genAddress n =
  Address
    <$> arbitrary -- addressCity :: Text
    <*> arbitraryReducedMaybe n -- addressCompany :: Maybe Text
    <*> arbitrary -- addressCountry :: Text
    <*> arbitraryReducedMaybe n -- addressEmail :: Maybe Text
    <*> arbitrary -- addressName :: Text
    <*> arbitraryReducedMaybe n -- addressPhone :: Maybe Text
    <*> arbitrary -- addressStreet :: Text
    <*> arbitrary -- addressStreetNumber :: Text
    <*> arbitrary -- addressZip :: Text
  
instance Arbitrary AiConfigDto where
  arbitrary = sized genAiConfigDto

genAiConfigDto :: Int -> Gen AiConfigDto
genAiConfigDto n =
  AiConfigDto
    <$> arbitraryReducedMaybe n -- aiConfigDtoAutoReply :: Maybe Bool
    <*> arbitraryReducedMaybe n -- aiConfigDtoMaxToolCalls :: Maybe Int
    <*> arbitrary -- aiConfigDtoModel :: Text
    <*> arbitrary -- aiConfigDtoName :: Text
    <*> arbitrary -- aiConfigDtoProvider :: Text
    <*> arbitraryReducedMaybe n -- aiConfigDtoSystemPrompt :: Maybe Text
    <*> arbitraryReducedMaybe n -- aiConfigDtoTriggerOn :: Maybe [Text]
  
instance Arbitrary AiSuggestion where
  arbitrary = sized genAiSuggestion

genAiSuggestion :: Int -> Gen AiSuggestion
genAiSuggestion n =
  AiSuggestion
    <$> arbitrary -- aiSuggestionConfidence :: Double
    <*> arbitrary -- aiSuggestionReasoning :: Text
    <*> arbitraryReducedMaybe n -- aiSuggestionSuggestedPriority :: Maybe Text
    <*> arbitrary -- aiSuggestionSuggestedReply :: Text
    <*> arbitraryReducedMaybe n -- aiSuggestionSuggestedStatus :: Maybe Text
    <*> arbitrary -- aiSuggestionToolCalls :: [Text]
  
instance Arbitrary AiSuggestionRequest where
  arbitrary = sized genAiSuggestionRequest

genAiSuggestionRequest :: Int -> Gen AiSuggestionRequest
genAiSuggestionRequest n =
  AiSuggestionRequest
    <$> arbitraryReducedMaybe n -- aiSuggestionRequestInstructions :: Maybe Text
    <*> arbitraryReducedMaybe n -- aiSuggestionRequestMessageBody :: Maybe Text
    <*> arbitrary -- aiSuggestionRequestTicketId :: Text
  
instance Arbitrary AiWorkerConfig where
  arbitrary = sized genAiWorkerConfig

genAiWorkerConfig :: Int -> Gen AiWorkerConfig
genAiWorkerConfig n =
  AiWorkerConfig
    <$> arbitrary -- aiWorkerConfigAutoReply :: Bool
    <*> arbitraryReduced n -- aiWorkerConfigCreatedAt :: DateTime
    <*> arbitrary -- aiWorkerConfigId :: Text
    <*> arbitrary -- aiWorkerConfigIsActive :: Bool
    <*> arbitrary -- aiWorkerConfigMaxToolCalls :: Int
    <*> arbitrary -- aiWorkerConfigModel :: Text
    <*> arbitrary -- aiWorkerConfigName :: Text
    <*> arbitrary -- aiWorkerConfigProvider :: Text
    <*> arbitrary -- aiWorkerConfigSystemPrompt :: Text
    <*> arbitrary -- aiWorkerConfigTenantId :: Text
    <*> arbitrary -- aiWorkerConfigTriggerOn :: [Text]
    <*> arbitraryReducedMaybe n -- aiWorkerConfigUpdatedAt :: Maybe DateTime
  
instance Arbitrary AllocatePaymentRequest where
  arbitrary = sized genAllocatePaymentRequest

genAllocatePaymentRequest :: Int -> Gen AllocatePaymentRequest
genAllocatePaymentRequest n =
  AllocatePaymentRequest
    <$> arbitrary -- allocatePaymentRequestAmount :: Double
    <*> arbitrary -- allocatePaymentRequestInvoiceId :: Text
    <*> arbitrary -- allocatePaymentRequestPaymentId :: Text
  
instance Arbitrary AnlageGErgebnis where
  arbitrary = sized genAnlageGErgebnis

genAnlageGErgebnis :: Int -> Gen AnlageGErgebnis
genAnlageGErgebnis n =
  AnlageGErgebnis
    <$> arbitrary -- anlageGErgebnisGewinnVerlust :: Text
    <*> arbitrary -- anlageGErgebnisGewstGezahlt :: Text
    <*> arbitrary -- anlageGErgebnisGewstMessbetragApprox :: Text
    <*> arbitrary -- anlageGErgebnisGewstPflichtig :: Bool
    <*> arbitrary -- anlageGErgebnisJahr :: Int
    <*> arbitraryReduced n -- anlageGErgebnisKfzHinweise :: [AnlageGKfzHinweis]
  
instance Arbitrary AnlageGKfzHinweis where
  arbitrary = sized genAnlageGKfzHinweis

genAnlageGKfzHinweis :: Int -> Gen AnlageGKfzHinweis
genAnlageGKfzHinweis n =
  AnlageGKfzHinweis
    <$> arbitrary -- anlageGKfzHinweisBezeichnung :: Text
    <*> arbitrary -- anlageGKfzHinweisKennzeichen :: Text
    <*> arbitrary -- anlageGKfzHinweisPrivatAnteilProzent :: Text
  
instance Arbitrary AnlageSErgebnis where
  arbitrary = sized genAnlageSErgebnis

genAnlageSErgebnis :: Int -> Gen AnlageSErgebnis
genAnlageSErgebnis n =
  AnlageSErgebnis
    <$> arbitrary -- anlageSErgebnisGewinnVerlust :: Text
    <*> arbitrary -- anlageSErgebnisJahr :: Int
    <*> arbitraryReduced n -- anlageSErgebnisKfzHinweise :: [AnlageSKfzHinweis]
  
instance Arbitrary AnlageSKfzHinweis where
  arbitrary = sized genAnlageSKfzHinweis

genAnlageSKfzHinweis :: Int -> Gen AnlageSKfzHinweis
genAnlageSKfzHinweis n =
  AnlageSKfzHinweis
    <$> arbitrary -- anlageSKfzHinweisBezeichnung :: Text
    <*> arbitrary -- anlageSKfzHinweisKennzeichen :: Text
    <*> arbitrary -- anlageSKfzHinweisPrivatAnteilProzent :: Text
  
instance Arbitrary ApiResponseGdprExport where
  arbitrary = sized genApiResponseGdprExport

genApiResponseGdprExport :: Int -> Gen ApiResponseGdprExport
genApiResponseGdprExport n =
  ApiResponseGdprExport
    <$> arbitraryReducedMaybe n -- apiResponseGdprExportData :: Maybe ApiResponseGdprExportData
    <*> arbitraryReducedMaybe n -- apiResponseGdprExportError :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseGdprExportMessage :: Maybe Text
    <*> arbitrary -- apiResponseGdprExportSuccess :: Bool
  
instance Arbitrary ApiResponseGdprExportData where
  arbitrary = sized genApiResponseGdprExportData

genApiResponseGdprExportData :: Int -> Gen ApiResponseGdprExportData
genApiResponseGdprExportData n =
  ApiResponseGdprExportData
    <$> arbitraryReduced n -- apiResponseGdprExportDataActivityLog :: [GdprActivity]
    <*> arbitraryReduced n -- apiResponseGdprExportDataApiKeys :: [GdprApiKey]
    <*> arbitraryReduced n -- apiResponseGdprExportDataBilling :: [GdprBillingInfo]
    <*> arbitraryReduced n -- apiResponseGdprExportDataExportedAt :: DateTime
    <*> arbitrary -- apiResponseGdprExportDataGeneratedByAi :: Bool
    <*> arbitraryReduced n -- apiResponseGdprExportDataNotifications :: [GdprNotification]
    <*> arbitraryReduced n -- apiResponseGdprExportDataRefreshTokens :: [GdprRefreshToken]
    <*> arbitraryReduced n -- apiResponseGdprExportDataTenants :: [GdprTenant]
    <*> arbitraryReduced n -- apiResponseGdprExportDataUsageEvents :: [GdprUsageEvent]
    <*> arbitraryReduced n -- apiResponseGdprExportDataUser :: GdprUser
  
instance Arbitrary ApiResponseString where
  arbitrary = sized genApiResponseString

genApiResponseString :: Int -> Gen ApiResponseString
genApiResponseString n =
  ApiResponseString
    <$> arbitraryReducedMaybe n -- apiResponseStringData :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseStringError :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseStringMessage :: Maybe Text
    <*> arbitrary -- apiResponseStringSuccess :: Bool
  
instance Arbitrary ApiResponseSubscriptionOverview where
  arbitrary = sized genApiResponseSubscriptionOverview

genApiResponseSubscriptionOverview :: Int -> Gen ApiResponseSubscriptionOverview
genApiResponseSubscriptionOverview n =
  ApiResponseSubscriptionOverview
    <$> arbitraryReducedMaybe n -- apiResponseSubscriptionOverviewData :: Maybe ApiResponseSubscriptionOverviewData
    <*> arbitraryReducedMaybe n -- apiResponseSubscriptionOverviewError :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseSubscriptionOverviewMessage :: Maybe Text
    <*> arbitrary -- apiResponseSubscriptionOverviewSuccess :: Bool
  
instance Arbitrary ApiResponseSubscriptionOverviewData where
  arbitrary = sized genApiResponseSubscriptionOverviewData

genApiResponseSubscriptionOverviewData :: Int -> Gen ApiResponseSubscriptionOverviewData
genApiResponseSubscriptionOverviewData n =
  ApiResponseSubscriptionOverviewData
    <$> arbitraryReducedMaybe n -- apiResponseSubscriptionOverviewDataCurrentPeriodEnd :: Maybe DateTime
    <*> arbitraryReduced n -- apiResponseSubscriptionOverviewDataFeatures :: PlanFeatures
    <*> arbitrary -- apiResponseSubscriptionOverviewDataIsTrialing :: Bool
    <*> arbitraryReduced n -- apiResponseSubscriptionOverviewDataLimits :: PlanLimits
    <*> arbitraryReducedMaybe n -- apiResponseSubscriptionOverviewDataManageUrl :: Maybe Text
    <*> arbitrary -- apiResponseSubscriptionOverviewDataPlan :: Text
    <*> arbitrary -- apiResponseSubscriptionOverviewDataPlanName :: Text
    <*> arbitrary -- apiResponseSubscriptionOverviewDataPriceEur :: Double
    <*> arbitraryReducedMaybe n -- apiResponseSubscriptionOverviewDataQuantity :: Maybe Int
    <*> arbitraryReducedMaybe n -- apiResponseSubscriptionOverviewDataStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseSubscriptionOverviewDataSubscriptionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseSubscriptionOverviewDataTrialEndsAt :: Maybe DateTime
    <*> arbitraryReduced n -- apiResponseSubscriptionOverviewDataUsage :: UsageSnapshot
  
instance Arbitrary ApiResponseTeam where
  arbitrary = sized genApiResponseTeam

genApiResponseTeam :: Int -> Gen ApiResponseTeam
genApiResponseTeam n =
  ApiResponseTeam
    <$> arbitraryReducedMaybe n -- apiResponseTeamData :: Maybe ApiResponseTeamData
    <*> arbitraryReducedMaybe n -- apiResponseTeamError :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseTeamMessage :: Maybe Text
    <*> arbitrary -- apiResponseTeamSuccess :: Bool
  
instance Arbitrary ApiResponseTeamData where
  arbitrary = sized genApiResponseTeamData

genApiResponseTeamData :: Int -> Gen ApiResponseTeamData
genApiResponseTeamData n =
  ApiResponseTeamData
    <$> arbitraryReduced n -- apiResponseTeamDataCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- apiResponseTeamDataDescription :: Maybe Text
    <*> arbitrary -- apiResponseTeamDataId :: Text
    <*> arbitrary -- apiResponseTeamDataName :: Text
    <*> arbitraryReducedMaybe n -- apiResponseTeamDataParentTeamId :: Maybe Text
    <*> arbitrary -- apiResponseTeamDataTenantId :: Text
    <*> arbitraryReduced n -- apiResponseTeamDataUpdatedAt :: DateTime
  
instance Arbitrary ApiResponseUserProfile where
  arbitrary = sized genApiResponseUserProfile

genApiResponseUserProfile :: Int -> Gen ApiResponseUserProfile
genApiResponseUserProfile n =
  ApiResponseUserProfile
    <$> arbitraryReducedMaybe n -- apiResponseUserProfileData :: Maybe ApiResponseUserProfileData
    <*> arbitraryReducedMaybe n -- apiResponseUserProfileError :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseUserProfileMessage :: Maybe Text
    <*> arbitrary -- apiResponseUserProfileSuccess :: Bool
  
instance Arbitrary ApiResponseUserProfileData where
  arbitrary = sized genApiResponseUserProfileData

genApiResponseUserProfileData :: Int -> Gen ApiResponseUserProfileData
genApiResponseUserProfileData n =
  ApiResponseUserProfileData
    <$> arbitraryReduced n -- apiResponseUserProfileDataCreatedAt :: DateTime
    <*> arbitrary -- apiResponseUserProfileDataEmail :: Text
    <*> arbitrary -- apiResponseUserProfileDataEmailVerified :: Bool
    <*> arbitrary -- apiResponseUserProfileDataFirstName :: Text
    <*> arbitrary -- apiResponseUserProfileDataFullName :: Text
    <*> arbitrary -- apiResponseUserProfileDataId :: Text
    <*> arbitrary -- apiResponseUserProfileDataLastName :: Text
  
instance Arbitrary ApiResponseVecPlan where
  arbitrary = sized genApiResponseVecPlan

genApiResponseVecPlan :: Int -> Gen ApiResponseVecPlan
genApiResponseVecPlan n =
  ApiResponseVecPlan
    <$> arbitraryReducedMaybe n -- apiResponseVecPlanData :: Maybe [ApiResponseVecPlanDataInner]
    <*> arbitraryReducedMaybe n -- apiResponseVecPlanError :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseVecPlanMessage :: Maybe Text
    <*> arbitrary -- apiResponseVecPlanSuccess :: Bool
  
instance Arbitrary ApiResponseVecPlanDataInner where
  arbitrary = sized genApiResponseVecPlanDataInner

genApiResponseVecPlanDataInner :: Int -> Gen ApiResponseVecPlanDataInner
genApiResponseVecPlanDataInner n =
  ApiResponseVecPlanDataInner
    <$> arbitraryReduced n -- apiResponseVecPlanDataInnerFeatures :: PlanFeatures
    <*> arbitrary -- apiResponseVecPlanDataInnerId :: Text
    <*> arbitraryReduced n -- apiResponseVecPlanDataInnerLimits :: PlanLimits
    <*> arbitrary -- apiResponseVecPlanDataInnerName :: Text
    <*> arbitrary -- apiResponseVecPlanDataInnerPriceEur :: Double
  
instance Arbitrary ApiResponseVecTeam where
  arbitrary = sized genApiResponseVecTeam

genApiResponseVecTeam :: Int -> Gen ApiResponseVecTeam
genApiResponseVecTeam n =
  ApiResponseVecTeam
    <$> arbitraryReducedMaybe n -- apiResponseVecTeamData :: Maybe [ApiResponseTeamData]
    <*> arbitraryReducedMaybe n -- apiResponseVecTeamError :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseVecTeamMessage :: Maybe Text
    <*> arbitrary -- apiResponseVecTeamSuccess :: Bool
  
instance Arbitrary ApiResponseVecUserTenantInfo where
  arbitrary = sized genApiResponseVecUserTenantInfo

genApiResponseVecUserTenantInfo :: Int -> Gen ApiResponseVecUserTenantInfo
genApiResponseVecUserTenantInfo n =
  ApiResponseVecUserTenantInfo
    <$> arbitraryReducedMaybe n -- apiResponseVecUserTenantInfoData :: Maybe [ApiResponseVecUserTenantInfoDataInner]
    <*> arbitraryReducedMaybe n -- apiResponseVecUserTenantInfoError :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiResponseVecUserTenantInfoMessage :: Maybe Text
    <*> arbitrary -- apiResponseVecUserTenantInfoSuccess :: Bool
  
instance Arbitrary ApiResponseVecUserTenantInfoDataInner where
  arbitrary = sized genApiResponseVecUserTenantInfoDataInner

genApiResponseVecUserTenantInfoDataInner :: Int -> Gen ApiResponseVecUserTenantInfoDataInner
genApiResponseVecUserTenantInfoDataInner n =
  ApiResponseVecUserTenantInfoDataInner
    <$> arbitraryReducedMaybe n -- apiResponseVecUserTenantInfoDataInnerCustomDomain :: Maybe Text
    <*> arbitrary -- apiResponseVecUserTenantInfoDataInnerRole :: Text
    <*> arbitraryReducedMaybe n -- apiResponseVecUserTenantInfoDataInnerSubdomain :: Maybe Text
    <*> arbitrary -- apiResponseVecUserTenantInfoDataInnerTenantId :: Text
    <*> arbitrary -- apiResponseVecUserTenantInfoDataInnerTenantName :: Text
  
instance Arbitrary ApplicationFilter where
  arbitrary = sized genApplicationFilter

genApplicationFilter :: Int -> Gen ApplicationFilter
genApplicationFilter n =
  ApplicationFilter
    <$> arbitraryReducedMaybe n -- applicationFilterPage :: Maybe Int
    <*> arbitraryReducedMaybe n -- applicationFilterPageSize :: Maybe Int
    <*> arbitraryReducedMaybe n -- applicationFilterPostingId :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationFilterStatus :: Maybe Text
  
instance Arbitrary ApplicationStatusDto where
  arbitrary = sized genApplicationStatusDto

genApplicationStatusDto :: Int -> Gen ApplicationStatusDto
genApplicationStatusDto n =
  ApplicationStatusDto
    <$> arbitraryReducedMaybe n -- applicationStatusDtoPostingId :: Maybe Text
    <*> arbitrary -- applicationStatusDtoStatus :: Text
  
instance Arbitrary AppointmentStatusUpdate where
  arbitrary = sized genAppointmentStatusUpdate

genAppointmentStatusUpdate :: Int -> Gen AppointmentStatusUpdate
genAppointmentStatusUpdate n =
  AppointmentStatusUpdate
    <$> arbitrary -- appointmentStatusUpdateStatus :: Text
  
instance Arbitrary Attachment where
  arbitrary = sized genAttachment

genAttachment :: Int -> Gen Attachment
genAttachment n =
  Attachment
    <$> arbitraryReducedMaybe n -- attachmentContactId :: Maybe Text
    <*> arbitrary -- attachmentFileName :: Text
    <*> arbitraryReducedMaybe n -- attachmentFileSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- attachmentMimeType :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentOcrText :: Maybe Text
    <*> arbitrary -- attachmentOriginalName :: Text
    <*> arbitraryReducedMaybe n -- attachmentPdfaPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentSha256Hash :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentUploadedBy :: Maybe Text
  
instance Arbitrary AttachmentCreate where
  arbitrary = sized genAttachmentCreate

genAttachmentCreate :: Int -> Gen AttachmentCreate
genAttachmentCreate n =
  AttachmentCreate
    <$> arbitraryReducedMaybe n -- attachmentCreateContactId :: Maybe Text
    <*> arbitrary -- attachmentCreateFileName :: Text
    <*> arbitraryReducedMaybe n -- attachmentCreateFileSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- attachmentCreateMimeType :: Maybe Text
    <*> arbitrary -- attachmentCreateOriginalName :: Text
    <*> arbitraryReducedMaybe n -- attachmentCreatePdfaPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentCreateSha256Hash :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentCreateUploadedBy :: Maybe Text
  
instance Arbitrary AttachmentVersion where
  arbitrary = sized genAttachmentVersion

genAttachmentVersion :: Int -> Gen AttachmentVersion
genAttachmentVersion n =
  AttachmentVersion
    <$> arbitrary -- attachmentVersionAttachmentId :: Text
    <*> arbitrary -- attachmentVersionFileName :: Text
    <*> arbitraryReducedMaybe n -- attachmentVersionFileSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- attachmentVersionMimeType :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentVersionOriginalName :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentVersionSha256Hash :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentVersionUploadedBy :: Maybe Text
    <*> arbitrary -- attachmentVersionVersionNumber :: Int
  
instance Arbitrary AuthResponse where
  arbitrary = sized genAuthResponse

genAuthResponse :: Int -> Gen AuthResponse
genAuthResponse n =
  AuthResponse
    <$> arbitraryReducedMaybe n -- authResponseAccessToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- authResponseMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- authResponseRefreshToken :: Maybe Text
    <*> arbitrary -- authResponseSuccess :: Bool
    <*> arbitraryReducedMaybe n -- authResponseUser :: Maybe Model
  
instance Arbitrary Automation where
  arbitrary = sized genAutomation

genAutomation :: Int -> Gen Automation
genAutomation n =
  Automation
    <$> arbitrary -- automationAutomationKey :: Text
    <*> arbitraryReduced n -- automationConfig :: AnyType
    <*> arbitraryReduced n -- automationCreatedAt :: DateTime
    <*> arbitrary -- automationEnabled :: Bool
    <*> arbitraryReducedMaybe n -- automationLastRunAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- automationNextRunAt :: Maybe DateTime
    <*> arbitrary -- automationTenantId :: Text
    <*> arbitraryReduced n -- automationUpdatedAt :: DateTime
  
instance Arbitrary AutomationDto where
  arbitrary = sized genAutomationDto

genAutomationDto :: Int -> Gen AutomationDto
genAutomationDto n =
  AutomationDto
    <$> arbitrary -- automationDtoAutomationKey :: Text
    <*> arbitraryReduced n -- automationDtoConfig :: AnyType
    <*> arbitraryReducedMaybe n -- automationDtoDefaultDay :: Maybe Int
    <*> arbitrary -- automationDtoDescription :: Text
    <*> arbitrary -- automationDtoEnabled :: Bool
    <*> arbitrary -- automationDtoKind :: Text
    <*> arbitraryReducedMaybe n -- automationDtoLastRunAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- automationDtoNextRunAt :: Maybe DateTime
    <*> arbitrary -- automationDtoScheduleKind :: Text
  
instance Arbitrary BWAExpenses where
  arbitrary = sized genBWAExpenses

genBWAExpenses :: Int -> Gen BWAExpenses
genBWAExpenses n =
  BWAExpenses
    <$> arbitraryReduced n -- bWAExpensesExpenseBreakdown :: [ExpenseItem]
    <*> arbitrary -- bWAExpensesTotalExpenses :: Text
  
instance Arbitrary BWAReport where
  arbitrary = sized genBWAReport

genBWAReport :: Int -> Gen BWAReport
genBWAReport n =
  BWAReport
    <$> arbitraryReduced n -- bWAReportExpenses :: BWAExpenses
    <*> arbitrary -- bWAReportGeneratedAt :: Text
    <*> arbitrary -- bWAReportPeriod :: Text
    <*> arbitraryReduced n -- bWAReportRevenue :: BWARevenue
    <*> arbitraryReduced n -- bWAReportSummary :: BWASummary
  
instance Arbitrary BWARevenue where
  arbitrary = sized genBWARevenue

genBWARevenue :: Int -> Gen BWARevenue
genBWARevenue n =
  BWARevenue
    <$> arbitraryReduced n -- bWARevenueRevenueBreakdown :: [RevenueItem]
    <*> arbitrary -- bWARevenueTotalRevenue :: Text
  
instance Arbitrary BWASummary where
  arbitrary = sized genBWASummary

genBWASummary :: Int -> Gen BWASummary
genBWASummary n =
  BWASummary
    <$> arbitrary -- bWASummaryGrossProfit :: Text
    <*> arbitrary -- bWASummaryNetProfit :: Text
    <*> arbitrary -- bWASummaryOpenInvoicesCount :: Integer
    <*> arbitrary -- bWASummaryOpenInvoicesTotal :: Text
    <*> arbitrary -- bWASummaryOverdueInvoicesCount :: Integer
    <*> arbitrary -- bWASummaryOverdueInvoicesTotal :: Text
    <*> arbitrary -- bWASummaryProfitMargin :: Double
  
instance Arbitrary BalanceItem where
  arbitrary = sized genBalanceItem

genBalanceItem :: Int -> Gen BalanceItem
genBalanceItem n =
  BalanceItem
    <$> arbitrary -- balanceItemAccount :: Text
    <*> arbitrary -- balanceItemAccountName :: Text
    <*> arbitrary -- balanceItemAmount :: Text
  
instance Arbitrary BalanceSheet where
  arbitrary = sized genBalanceSheet

genBalanceSheet :: Int -> Gen BalanceSheet
genBalanceSheet n =
  BalanceSheet
    <$> arbitraryReduced n -- balanceSheetAssets :: [BalanceItem]
    <*> arbitrary -- balanceSheetBalanced :: Bool
    <*> arbitraryReduced n -- balanceSheetEquityLiabilities :: [BalanceItem]
    <*> arbitrary -- balanceSheetTotalAssets :: Text
    <*> arbitrary -- balanceSheetTotalEquityLiabilities :: Text
  
instance Arbitrary BankLookup where
  arbitrary = sized genBankLookup

genBankLookup :: Int -> Gen BankLookup
genBankLookup n =
  BankLookup
    <$> arbitraryReducedMaybe n -- bankLookupBankName :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankLookupBic :: Maybe Text
    <*> arbitrary -- bankLookupIban :: Text
    <*> arbitraryReducedMaybe n -- bankLookupNextgenpsd2Url :: Maybe Text
    <*> arbitrary -- bankLookupPsd2Supported :: Bool
  
instance Arbitrary Betriebsstaette where
  arbitrary = sized genBetriebsstaette

genBetriebsstaette :: Int -> Gen Betriebsstaette
genBetriebsstaette n =
  Betriebsstaette
    <$> arbitrary -- betriebsstaetteBeschaefigte :: Integer
    <*> arbitrary -- betriebsstaetteName :: Text
  
instance Arbitrary BetriebsstaettenDetail where
  arbitrary = sized genBetriebsstaettenDetail

genBetriebsstaettenDetail :: Int -> Gen BetriebsstaettenDetail
genBetriebsstaettenDetail n =
  BetriebsstaettenDetail
    <$> arbitrary -- betriebsstaettenDetailBeschaefigte :: Integer
    <*> arbitrary -- betriebsstaettenDetailMonatlicherBeitrag :: Text
    <*> arbitrary -- betriebsstaettenDetailName :: Text
  
instance Arbitrary BilanzItem where
  arbitrary = sized genBilanzItem

genBilanzItem :: Int -> Gen BilanzItem
genBilanzItem n =
  BilanzItem
    <$> arbitrary -- bilanzItemAmount :: Text
    <*> arbitrary -- bilanzItemKonto :: Text
    <*> arbitrary -- bilanzItemName :: Text
  
instance Arbitrary BilanzReport where
  arbitrary = sized genBilanzReport

genBilanzReport :: Int -> Gen BilanzReport
genBilanzReport n =
  BilanzReport
    <$> arbitraryReduced n -- bilanzReportAktiva :: [BilanzItem]
    <*> arbitrary -- bilanzReportBalanced :: Bool
    <*> arbitrary -- bilanzReportGeneratedAt :: Text
    <*> arbitraryReduced n -- bilanzReportPassiva :: [BilanzItem]
    <*> arbitrary -- bilanzReportPeriod :: Text
    <*> arbitrary -- bilanzReportTotalAktiva :: Text
    <*> arbitrary -- bilanzReportTotalPassiva :: Text
  
instance Arbitrary Bom where
  arbitrary = sized genBom

genBom :: Int -> Gen Bom
genBom n =
  Bom
    <$> arbitraryReducedMaybe n -- bomComponents :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- bomDescription :: Maybe Text
    <*> arbitrary -- bomName :: Text
    <*> arbitraryReducedMaybe n -- bomOutputQuantity :: Maybe Integer
    <*> arbitrary -- bomProductId :: Text
    <*> arbitraryReducedMaybe n -- bomStatus :: Maybe BomStatus
  
instance Arbitrary BomCreate where
  arbitrary = sized genBomCreate

genBomCreate :: Int -> Gen BomCreate
genBomCreate n =
  BomCreate
    <$> arbitraryReducedMaybe n -- bomCreateComponents :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- bomCreateDescription :: Maybe Text
    <*> arbitrary -- bomCreateName :: Text
    <*> arbitraryReducedMaybe n -- bomCreateOutputQuantity :: Maybe Integer
    <*> arbitrary -- bomCreateProductId :: Text
    <*> arbitraryReducedMaybe n -- bomCreateStatus :: Maybe BomStatus
  
instance Arbitrary BomUpdate where
  arbitrary = sized genBomUpdate

genBomUpdate :: Int -> Gen BomUpdate
genBomUpdate n =
  BomUpdate
    <$> arbitraryReducedMaybe n -- bomUpdateComponents :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- bomUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- bomUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- bomUpdateOutputQuantity :: Maybe Integer
    <*> arbitraryReducedMaybe n -- bomUpdateProductId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bomUpdateStatus :: Maybe BomStatus
  
instance Arbitrary BoxFit where
  arbitrary = sized genBoxFit

genBoxFit :: Int -> Gen BoxFit
genBoxFit n =
  BoxFit
    <$> arbitrary -- boxFitHeightCm :: Double
    <*> arbitrary -- boxFitItemCount :: Int
    <*> arbitrary -- boxFitLengthCm :: Double
    <*> arbitrary -- boxFitVolumeCm3 :: Double
    <*> arbitrary -- boxFitWidthCm :: Double
  
instance Arbitrary Budget where
  arbitrary = sized genBudget

genBudget :: Int -> Gen Budget
genBudget n =
  Budget
    <$> arbitrary -- budgetCategory :: Text
    <*> arbitrary -- budgetMonthlyGoal :: Text
    <*> arbitraryReducedMaybe n -- budgetUpdatedAt :: Maybe DateTime
    <*> arbitrary -- budgetYear :: Int
  
instance Arbitrary BudgetErgebnis where
  arbitrary = sized genBudgetErgebnis

genBudgetErgebnis :: Int -> Gen BudgetErgebnis
genBudgetErgebnis n =
  BudgetErgebnis
    <$> arbitrary -- budgetErgebnisJahr :: Int
    <*> arbitrary -- budgetErgebnisMonat :: Int
    <*> arbitraryReduced n -- budgetErgebnisMonatsBudget :: [BudgetKategorie]
    <*> arbitraryReduced n -- budgetErgebnisPrognoseRestjahr :: [BudgetKategorie]
  
instance Arbitrary BudgetGoalRequest where
  arbitrary = sized genBudgetGoalRequest

genBudgetGoalRequest :: Int -> Gen BudgetGoalRequest
genBudgetGoalRequest n =
  BudgetGoalRequest
    <$> arbitrary -- budgetGoalRequestMonthlyGoal :: Text
    <*> arbitrary -- budgetGoalRequestYear :: Int
  
instance Arbitrary BudgetKategorie where
  arbitrary = sized genBudgetKategorie

genBudgetKategorie :: Int -> Gen BudgetKategorie
genBudgetKategorie n =
  BudgetKategorie
    <$> arbitrary -- budgetKategorieBudget :: Text
    <*> arbitrary -- budgetKategorieDifferenz :: Text
    <*> arbitraryReducedMaybe n -- budgetKategorieGoal :: Maybe Text
    <*> arbitrary -- budgetKategorieIst :: Text
    <*> arbitrary -- budgetKategorieKategorie :: Text
  
instance Arbitrary CartItemInput where
  arbitrary = sized genCartItemInput

genCartItemInput :: Int -> Gen CartItemInput
genCartItemInput n =
  CartItemInput
    <$> arbitrary -- cartItemInputProductId :: Text
    <*> arbitrary -- cartItemInputQuantity :: Int
  
instance Arbitrary CashflowReport where
  arbitrary = sized genCashflowReport

genCashflowReport :: Int -> Gen CashflowReport
genCashflowReport n =
  CashflowReport
    <$> arbitrary -- cashflowReportClosingBalance :: Double
    <*> arbitrary -- cashflowReportFinancingCashflow :: Double
    <*> arbitrary -- cashflowReportInvestingCashflow :: Double
    <*> arbitrary -- cashflowReportNetCashflow :: Double
    <*> arbitrary -- cashflowReportOpeningBalance :: Double
    <*> arbitrary -- cashflowReportOperatingCashflow :: Double
    <*> arbitrary -- cashflowReportPeriod :: Text
  
instance Arbitrary CategoryTotal where
  arbitrary = sized genCategoryTotal

genCategoryTotal :: Int -> Gen CategoryTotal
genCategoryTotal n =
  CategoryTotal
    <$> arbitrary -- categoryTotalCategoryId :: Text
    <*> arbitrary -- categoryTotalSharePct :: Double
    <*> arbitrary -- categoryTotalTco2e :: Text
  
instance Arbitrary ChangePasswordRequest where
  arbitrary = sized genChangePasswordRequest

genChangePasswordRequest :: Int -> Gen ChangePasswordRequest
genChangePasswordRequest n =
  ChangePasswordRequest
    <$> arbitrary -- changePasswordRequestCurrentPassword :: Text
    <*> arbitrary -- changePasswordRequestNewPassword :: Text
  
instance Arbitrary ChangelogEntry where
  arbitrary = sized genChangelogEntry

genChangelogEntry :: Int -> Gen ChangelogEntry
genChangelogEntry n =
  ChangelogEntry
    <$> arbitrary -- changelogEntryDate :: Text
    <*> arbitrary -- changelogEntryNotes :: Text
    <*> arbitrary -- changelogEntryVersion :: Text
  
instance Arbitrary ComplianceEntry where
  arbitrary = sized genComplianceEntry

genComplianceEntry :: Int -> Gen ComplianceEntry
genComplianceEntry n =
  ComplianceEntry
    <$> arbitrary -- complianceEntryDescription :: Text
    <*> arbitrary -- complianceEntryModule :: Text
    <*> arbitrary -- complianceEntryRegulations :: [Text]
  
instance Arbitrary ComplianceTraining where
  arbitrary = sized genComplianceTraining

genComplianceTraining :: Int -> Gen ComplianceTraining
genComplianceTraining n =
  ComplianceTraining
    <$> arbitraryReducedMaybe n -- complianceTrainingAssignable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- complianceTrainingCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- complianceTrainingDeletedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- complianceTrainingDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingId :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingPassScore :: Maybe Int
    <*> arbitraryReducedMaybe n -- complianceTrainingPluginPlatform :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingSource :: Maybe TrainingSource
    <*> arbitraryReducedMaybe n -- complianceTrainingTenantId :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- complianceTrainingValidityMonths :: Maybe Int
  
instance Arbitrary ComplianceTrainingCreate where
  arbitrary = sized genComplianceTrainingCreate

genComplianceTrainingCreate :: Int -> Gen ComplianceTrainingCreate
genComplianceTrainingCreate n =
  ComplianceTrainingCreate
    <$> arbitraryReducedMaybe n -- complianceTrainingCreateAssignable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- complianceTrainingCreateCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingCreateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingCreatePassScore :: Maybe Int
    <*> arbitraryReducedMaybe n -- complianceTrainingCreatePluginPlatform :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingCreateSource :: Maybe TrainingSource
    <*> arbitraryReducedMaybe n -- complianceTrainingCreateTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingCreateValidityMonths :: Maybe Int
  
instance Arbitrary ComplianceTrainingUpdate where
  arbitrary = sized genComplianceTrainingUpdate

genComplianceTrainingUpdate :: Int -> Gen ComplianceTrainingUpdate
genComplianceTrainingUpdate n =
  ComplianceTrainingUpdate
    <$> arbitraryReducedMaybe n -- complianceTrainingUpdateAssignable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- complianceTrainingUpdateCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingUpdatePassScore :: Maybe Int
    <*> arbitraryReducedMaybe n -- complianceTrainingUpdatePluginPlatform :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingUpdateSource :: Maybe TrainingSource
    <*> arbitraryReducedMaybe n -- complianceTrainingUpdateTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- complianceTrainingUpdateValidityMonths :: Maybe Int
  
instance Arbitrary ConfigFieldInfo where
  arbitrary = sized genConfigFieldInfo

genConfigFieldInfo :: Int -> Gen ConfigFieldInfo
genConfigFieldInfo n =
  ConfigFieldInfo
    <$> arbitraryReduced n -- configFieldInfoKind :: ConfigFieldKind
    <*> arbitrary -- configFieldInfoLabel :: Text
    <*> arbitrary -- configFieldInfoName :: Text
    <*> arbitraryReducedMaybe n -- configFieldInfoPlaceholder :: Maybe Text
    <*> arbitrary -- configFieldInfoRequired :: Bool
  
instance Arbitrary ConfigFieldKind where
  arbitrary = sized genConfigFieldKind

genConfigFieldKind :: Int -> Gen ConfigFieldKind
genConfigFieldKind n =
  ConfigFieldKind
    <$> arbitrary -- configFieldKindType :: E'Type
    <*> arbitrary -- configFieldKindOptions :: [Text]
  
instance Arbitrary ConfigFieldKindOneOf where
  arbitrary = sized genConfigFieldKindOneOf

genConfigFieldKindOneOf :: Int -> Gen ConfigFieldKindOneOf
genConfigFieldKindOneOf n =
  ConfigFieldKindOneOf
    <$> arbitrary -- configFieldKindOneOfType :: E'Type3
  
instance Arbitrary ConfigFieldKindOneOf1 where
  arbitrary = sized genConfigFieldKindOneOf1

genConfigFieldKindOneOf1 :: Int -> Gen ConfigFieldKindOneOf1
genConfigFieldKindOneOf1 n =
  ConfigFieldKindOneOf1
    <$> arbitrary -- configFieldKindOneOf1Type :: E'Type4
  
instance Arbitrary ConfigFieldKindOneOf2 where
  arbitrary = sized genConfigFieldKindOneOf2

genConfigFieldKindOneOf2 :: Int -> Gen ConfigFieldKindOneOf2
genConfigFieldKindOneOf2 n =
  ConfigFieldKindOneOf2
    <$> arbitrary -- configFieldKindOneOf2Type :: E'Type5
  
instance Arbitrary ConfigFieldKindOneOf3 where
  arbitrary = sized genConfigFieldKindOneOf3

genConfigFieldKindOneOf3 :: Int -> Gen ConfigFieldKindOneOf3
genConfigFieldKindOneOf3 n =
  ConfigFieldKindOneOf3
    <$> arbitrary -- configFieldKindOneOf3Options :: [Text]
    <*> arbitrary -- configFieldKindOneOf3Type :: E'Type6
  
instance Arbitrary ConfigFieldKindOneOf4 where
  arbitrary = sized genConfigFieldKindOneOf4

genConfigFieldKindOneOf4 :: Int -> Gen ConfigFieldKindOneOf4
genConfigFieldKindOneOf4 n =
  ConfigFieldKindOneOf4
    <$> arbitrary -- configFieldKindOneOf4Type :: E'Type7
  
instance Arbitrary Contact where
  arbitrary = sized genContact

genContact :: Int -> Gen Contact
genContact n =
  Contact
    <$> arbitraryReducedMaybe n -- contactAccountHolder :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactAcquisitionCost :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactAddressSupplement :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactAttention :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactBankName :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactBic :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactBuyerReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCategory :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCertificateAuthority :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCertificateNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCertificateParagraph :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCertificateValidUntil :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCompanyName :: Maybe Text
    <*> arbitrary -- contactContactId :: Text
    <*> arbitraryReduced n -- contactContactPersons :: AnyType
    <*> arbitrary -- contactContactType :: Text
    <*> arbitraryReducedMaybe n -- contactCountry :: Maybe Text
    <*> arbitrary -- contactCreatedAt :: Text
    <*> arbitraryReducedMaybe n -- contactCreditLimit :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreditorAccountSkr03 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreditorAccountSkr04 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactDebitorAccountSkr03 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactDebitorAccountSkr04 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactDefaultDebitorNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactDeliveryBlock :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactDepartment :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactDiscountDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactDiscountPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactDonationReceiptEligible :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactExternalId :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactFax :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactIban :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactIndustry :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactIsMember :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactIsNonprofit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactLastContactDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactLastPurchaseDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactLeitwegId :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactLifetimeValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactMandateDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactMandateReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactMarketingConsent :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactMarketingConsentAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactMarketingConsentSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactMobile :: Maybe Text
    <*> arbitrary -- contactName :: Text
    <*> arbitraryReducedMaybe n -- contactNextContactDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactOpeningBalance :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactOpeningBalanceDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactOrderReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactPaymentBlock :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactPaymentGracePeriodDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactPaymentMethods :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- contactPaymentTerms :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactPhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactRating :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactSalesRepresentative :: Maybe Text
    <*> arbitraryReduced n -- contactSocialMedia :: AnyType
    <*> arbitraryReducedMaybe n -- contactSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactState :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactStreet :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactStreetNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactSupplierNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactTags :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- contactTaxCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactTaxNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactTaxOffice :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactTotalInvoices :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactTotalRevenue :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactVatId :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactVatIdValidated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactVatIdValidationDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactWebsite :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactZip :: Maybe Text
  
instance Arbitrary ContactCreate where
  arbitrary = sized genContactCreate

genContactCreate :: Int -> Gen ContactCreate
genContactCreate n =
  ContactCreate
    <$> arbitraryReducedMaybe n -- contactCreateAccountHolder :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateAcquisitionCost :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateAddressSupplement :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateAttention :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateBankName :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateBic :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateBuyerReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCategory :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCertificateAuthority :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCertificateNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCertificateParagraph :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCertificateValidUntil :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactCreateCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateContactPersons :: Maybe AnyType
    <*> arbitraryReduced n -- contactCreateContactType :: ContactType
    <*> arbitraryReducedMaybe n -- contactCreateCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- contactCreateCreditLimit :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCreditorAccountSkr03 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCreditorAccountSkr04 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateCustomFields :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- contactCreateCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateDebitorAccountSkr03 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateDebitorAccountSkr04 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateDefaultDebitorNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateDeliveryBlock :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactCreateDepartment :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateDiscountDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactCreateDiscountPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateDonationReceiptEligible :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactCreateEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateExternalId :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateFax :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateIban :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateIndustry :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactCreateIsMember :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactCreateIsNonprofit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactCreateLastContactDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactCreateLastPurchaseDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactCreateLeitwegId :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateLifetimeValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateMandateDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactCreateMandateReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateMarketingConsent :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactCreateMarketingConsentAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- contactCreateMarketingConsentSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateMobile :: Maybe Text
    <*> arbitrary -- contactCreateName :: Text
    <*> arbitraryReducedMaybe n -- contactCreateNextContactDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateOpeningBalance :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateOpeningBalanceDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactCreateOrderReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreatePaymentBlock :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactCreatePaymentGracePeriodDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactCreatePaymentMethods :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- contactCreatePaymentTerms :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreatePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateRating :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactCreateSalesRepresentative :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateSepaBatchBooking :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactCreateSepaSequenceType :: Maybe SepaSequenceType
    <*> arbitraryReducedMaybe n -- contactCreateSocialMedia :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- contactCreateSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateState :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateStreet :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateStreetNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateSupplierNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateTags :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- contactCreateTaxCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- contactCreateTaxNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateTaxOffice :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateTotalInvoices :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactCreateTotalRevenue :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateVatId :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateVatIdValidated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactCreateVatIdValidationDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactCreateWebsite :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactCreateZip :: Maybe Text
  
instance Arbitrary ContactHistoryResponse where
  arbitrary = sized genContactHistoryResponse

genContactHistoryResponse :: Int -> Gen ContactHistoryResponse
genContactHistoryResponse n =
  ContactHistoryResponse
    <$> arbitrary -- contactHistoryResponseContactId :: Text
    <*> arbitrary -- contactHistoryResponseInboundCount :: Integer
    <*> arbitraryReduced n -- contactHistoryResponseItems :: [CustomerCommunication]
    <*> arbitrary -- contactHistoryResponseOutboundCount :: Integer
  
instance Arbitrary ContactInfo where
  arbitrary = sized genContactInfo

genContactInfo :: Int -> Gen ContactInfo
genContactInfo n =
  ContactInfo
    <$> arbitrary -- contactInfoHint :: Text
    <*> arbitrary -- contactInfoHintEn :: Text
    <*> arbitrary -- contactInfoRole :: Text
    <*> arbitrary -- contactInfoRoleEn :: Text
  
instance Arbitrary ContactTimelineResponse where
  arbitrary = sized genContactTimelineResponse

genContactTimelineResponse :: Int -> Gen ContactTimelineResponse
genContactTimelineResponse n =
  ContactTimelineResponse
    <$> arbitrary -- contactTimelineResponseContactId :: Text
    <*> arbitraryReduced n -- contactTimelineResponseEvents :: [TimelineEvent]
  
instance Arbitrary ContactUpdate where
  arbitrary = sized genContactUpdate

genContactUpdate :: Int -> Gen ContactUpdate
genContactUpdate n =
  ContactUpdate
    <$> arbitraryReducedMaybe n -- contactUpdateAccountHolder :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateAcquisitionCost :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateAddressSupplement :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateAttention :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateBankName :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateBic :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateBuyerReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCategory :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCertificateAuthority :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCertificateNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCertificateParagraph :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCertificateValidUntil :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactUpdateCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateContactPersons :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- contactUpdateContactType :: Maybe ContactType
    <*> arbitraryReducedMaybe n -- contactUpdateCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- contactUpdateCreditLimit :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCreditorAccountSkr03 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCreditorAccountSkr04 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateCustomFields :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- contactUpdateCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateDebitorAccountSkr03 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateDebitorAccountSkr04 :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateDefaultDebitorNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateDeliveryBlock :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactUpdateDepartment :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateDiscountDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactUpdateDiscountPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateDonationReceiptEligible :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactUpdateEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateExternalId :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateFax :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateIban :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateIndustry :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactUpdateIsMember :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactUpdateIsNonprofit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactUpdateLastContactDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactUpdateLastPurchaseDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactUpdateLeitwegId :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateLifetimeValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateMandateDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactUpdateMandateReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateMarketingConsent :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactUpdateMarketingConsentAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- contactUpdateMarketingConsentSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateMobile :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateNextContactDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateOpeningBalance :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateOpeningBalanceDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactUpdateOrderReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdatePaymentBlock :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactUpdatePaymentGracePeriodDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactUpdatePaymentMethods :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- contactUpdatePaymentTerms :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdatePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateRating :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactUpdateSalesRepresentative :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateSepaBatchBooking :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactUpdateSepaSequenceType :: Maybe SepaSequenceType
    <*> arbitraryReducedMaybe n -- contactUpdateSocialMedia :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- contactUpdateSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateState :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateStreet :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateStreetNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateSupplierNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateTags :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- contactUpdateTaxCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- contactUpdateTaxNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateTaxOffice :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateTotalInvoices :: Maybe Int
    <*> arbitraryReducedMaybe n -- contactUpdateTotalRevenue :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateVatId :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateVatIdValidated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- contactUpdateVatIdValidationDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- contactUpdateWebsite :: Maybe Text
    <*> arbitraryReducedMaybe n -- contactUpdateZip :: Maybe Text
  
instance Arbitrary ConvertResponse where
  arbitrary = sized genConvertResponse

genConvertResponse :: Int -> Gen ConvertResponse
genConvertResponse n =
  ConvertResponse
    <$> arbitrary -- convertResponseInvoiceId :: Text
    <*> arbitrary -- convertResponseInvoiceNumber :: Text
    <*> arbitrary -- convertResponseProformaId :: Text
    <*> arbitrary -- convertResponseProformaNumber :: Text
  
instance Arbitrary CostingLine where
  arbitrary = sized genCostingLine

genCostingLine :: Int -> Gen CostingLine
genCostingLine n =
  CostingLine
    <$> arbitrary -- costingLineLineCost :: Text
    <*> arbitrary -- costingLineName :: Text
    <*> arbitrary -- costingLineProductId :: Text
    <*> arbitrary -- costingLineQuantityPerUnit :: Integer
    <*> arbitrary -- costingLineSku :: Text
    <*> arbitrary -- costingLineTotalQuantity :: Integer
    <*> arbitraryReducedMaybe n -- costingLineUnitPurchasePrice :: Maybe Text
  
instance Arbitrary Coupon where
  arbitrary = sized genCoupon

genCoupon :: Int -> Gen Coupon
genCoupon n =
  Coupon
    <$> arbitrary -- couponCode :: Text
    <*> arbitraryReducedMaybe n -- couponDescription :: Maybe Text
    <*> arbitraryReduced n -- couponDiscountType :: DiscountType
    <*> arbitrary -- couponDiscountValue :: Text
    <*> arbitraryReducedMaybe n -- couponExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- couponIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- couponIsCombineable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- couponMaxDiscountAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponMaxUses :: Maybe Int
    <*> arbitraryReducedMaybe n -- couponMaxUsesPerCustomer :: Maybe Int
    <*> arbitraryReducedMaybe n -- couponMinOrderAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponProductIds :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- couponStartsAt :: Maybe DateTime
  
instance Arbitrary CouponCreate where
  arbitrary = sized genCouponCreate

genCouponCreate :: Int -> Gen CouponCreate
genCouponCreate n =
  CouponCreate
    <$> arbitrary -- couponCreateCode :: Text
    <*> arbitraryReducedMaybe n -- couponCreateDescription :: Maybe Text
    <*> arbitraryReduced n -- couponCreateDiscountType :: DiscountType
    <*> arbitrary -- couponCreateDiscountValue :: Text
    <*> arbitraryReducedMaybe n -- couponCreateExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- couponCreateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- couponCreateIsCombineable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- couponCreateMaxDiscountAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponCreateMaxUses :: Maybe Int
    <*> arbitraryReducedMaybe n -- couponCreateMaxUsesPerCustomer :: Maybe Int
    <*> arbitraryReducedMaybe n -- couponCreateMinOrderAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponCreateProductIds :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- couponCreateStartsAt :: Maybe DateTime
  
instance Arbitrary CouponUpdate where
  arbitrary = sized genCouponUpdate

genCouponUpdate :: Int -> Gen CouponUpdate
genCouponUpdate n =
  CouponUpdate
    <$> arbitraryReducedMaybe n -- couponUpdateCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponUpdateDiscountType :: Maybe DiscountType
    <*> arbitraryReducedMaybe n -- couponUpdateDiscountValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponUpdateExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- couponUpdateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- couponUpdateIsCombineable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- couponUpdateMaxDiscountAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponUpdateMaxUses :: Maybe Int
    <*> arbitraryReducedMaybe n -- couponUpdateMaxUsesPerCustomer :: Maybe Int
    <*> arbitraryReducedMaybe n -- couponUpdateMinOrderAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponUpdateProductIds :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- couponUpdateStartsAt :: Maybe DateTime
  
instance Arbitrary CouponValidation where
  arbitrary = sized genCouponValidation

genCouponValidation :: Int -> Gen CouponValidation
genCouponValidation n =
  CouponValidation
    <$> arbitrary -- couponValidationCode :: Text
    <*> arbitrary -- couponValidationDiscountType :: Text
    <*> arbitrary -- couponValidationDiscountValue :: Text
    <*> arbitrary -- couponValidationDiscountedAmount :: Text
    <*> arbitraryReducedMaybe n -- couponValidationMaxDiscountAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- couponValidationReason :: Maybe Text
    <*> arbitrary -- couponValidationValid :: Bool
  
instance Arbitrary CreateChannelDto where
  arbitrary = sized genCreateChannelDto

genCreateChannelDto :: Int -> Gen CreateChannelDto
genCreateChannelDto n =
  CreateChannelDto
    <$> arbitrary -- createChannelDtoChannelType :: Text
    <*> arbitraryReduced n -- createChannelDtoConfig :: AnyType
    <*> arbitrary -- createChannelDtoName :: Text
  
instance Arbitrary CreateConnectionRequest where
  arbitrary = sized genCreateConnectionRequest

genCreateConnectionRequest :: Int -> Gen CreateConnectionRequest
genCreateConnectionRequest n =
  CreateConnectionRequest
    <$> arbitraryReducedMaybe n -- createConnectionRequestApiKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- createConnectionRequestApiSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- createConnectionRequestConfig :: Maybe AnyType
    <*> arbitrary -- createConnectionRequestLabel :: Text
    <*> arbitrary -- createConnectionRequestPlatform :: Text
    <*> arbitraryReducedMaybe n -- createConnectionRequestShopDomain :: Maybe Text
  
instance Arbitrary CreateEmissionEntry where
  arbitrary = sized genCreateEmissionEntry

genCreateEmissionEntry :: Int -> Gen CreateEmissionEntry
genCreateEmissionEntry n =
  CreateEmissionEntry
    <$> arbitrary -- createEmissionEntryActivityValue :: Text
    <*> arbitrary -- createEmissionEntryCategoryId :: Text
    <*> arbitrary -- createEmissionEntryDescription :: Text
    <*> arbitrary -- createEmissionEntryMethod :: Text
    <*> arbitrary -- createEmissionEntryScope :: Text
    <*> arbitrary -- createEmissionEntryUnit :: Text
    <*> arbitrary -- createEmissionEntryYear :: Int
  
instance Arbitrary CreateEmissionTarget where
  arbitrary = sized genCreateEmissionTarget

genCreateEmissionTarget :: Int -> Gen CreateEmissionTarget
genCreateEmissionTarget n =
  CreateEmissionTarget
    <$> arbitrary -- createEmissionTargetBaseValue :: Text
    <*> arbitrary -- createEmissionTargetBaseYear :: Int
    <*> arbitrary -- createEmissionTargetDescription :: Text
    <*> arbitrary -- createEmissionTargetScope :: Text
    <*> arbitrary -- createEmissionTargetTargetValue :: Text
    <*> arbitrary -- createEmissionTargetTargetYear :: Int
  
instance Arbitrary CreateShipmentRequest where
  arbitrary = sized genCreateShipmentRequest

genCreateShipmentRequest :: Int -> Gen CreateShipmentRequest
genCreateShipmentRequest n =
  CreateShipmentRequest
    <$> arbitrary -- createShipmentRequestCarrier :: Text
    <*> arbitraryReducedMaybe n -- createShipmentRequestService :: Maybe Text
    <*> arbitraryReducedMaybe n -- createShipmentRequestWeightKg :: Maybe Double
  
instance Arbitrary CreateSubscriptionRequest where
  arbitrary = sized genCreateSubscriptionRequest

genCreateSubscriptionRequest :: Int -> Gen CreateSubscriptionRequest
genCreateSubscriptionRequest n =
  CreateSubscriptionRequest
    <$> arbitrary -- createSubscriptionRequestEventType :: Text
    <*> arbitraryReducedMaybe n -- createSubscriptionRequestIsActive :: Maybe Bool
    <*> arbitrary -- createSubscriptionRequestName :: Text
    <*> arbitraryReducedMaybe n -- createSubscriptionRequestSecret :: Maybe Text
    <*> arbitrary -- createSubscriptionRequestUrl :: Text
  
instance Arbitrary CreateTicketRequest where
  arbitrary = sized genCreateTicketRequest

genCreateTicketRequest :: Int -> Gen CreateTicketRequest
genCreateTicketRequest n =
  CreateTicketRequest
    <$> arbitraryReducedMaybe n -- createTicketRequestChannelId :: Maybe Text
    <*> arbitraryReducedMaybe n -- createTicketRequestChannelType :: Maybe Text
    <*> arbitraryReducedMaybe n -- createTicketRequestCustomerEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- createTicketRequestCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- createTicketRequestCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createTicketRequestExternalId :: Maybe Text
    <*> arbitrary -- createTicketRequestMessageBody :: Text
    <*> arbitraryReducedMaybe n -- createTicketRequestOrderRef :: Maybe Text
    <*> arbitrary -- createTicketRequestSubject :: Text
  
instance Arbitrary CurrentInventoryValue where
  arbitrary = sized genCurrentInventoryValue

genCurrentInventoryValue :: Int -> Gen CurrentInventoryValue
genCurrentInventoryValue n =
  CurrentInventoryValue
    <$> arbitraryReduced n -- currentInventoryValueHistory :: [InventoryValuePoint]
    <*> arbitrary -- currentInventoryValueProductCount :: Integer
    <*> arbitrary -- currentInventoryValueTotalPurchaseValue :: Text
    <*> arbitrary -- currentInventoryValueTotalSalesValue :: Text
  
instance Arbitrary Customer where
  arbitrary = sized genCustomer

genCustomer :: Int -> Gen Customer
genCustomer n =
  Customer
    <$> arbitraryReducedMaybe n -- customerAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- customerContactPerson :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerExternalOrderNumber :: Maybe Text
    <*> arbitrary -- customerName :: Text
    <*> arbitraryReducedMaybe n -- customerPaymentGracePeriodDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- customerPhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerVatId :: Maybe Text
  
instance Arbitrary CustomerCommunication where
  arbitrary = sized genCustomerCommunication

genCustomerCommunication :: Int -> Gen CustomerCommunication
genCustomerCommunication n =
  CustomerCommunication
    <$> arbitraryReducedMaybe n -- customerCommunicationBody :: Maybe Text
    <*> arbitraryReduced n -- customerCommunicationChannel :: CommunicationChannel
    <*> arbitrary -- customerCommunicationContactId :: Text
    <*> arbitraryReducedMaybe n -- customerCommunicationCounterparty :: Maybe Text
    <*> arbitraryReduced n -- customerCommunicationDirection :: CommunicationDirection
    <*> arbitraryReducedMaybe n -- customerCommunicationOccurredAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- customerCommunicationSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerCommunicationTags :: Maybe AnyType
  
instance Arbitrary CustomerCommunicationCreate where
  arbitrary = sized genCustomerCommunicationCreate

genCustomerCommunicationCreate :: Int -> Gen CustomerCommunicationCreate
genCustomerCommunicationCreate n =
  CustomerCommunicationCreate
    <$> arbitraryReducedMaybe n -- customerCommunicationCreateBody :: Maybe Text
    <*> arbitraryReduced n -- customerCommunicationCreateChannel :: CommunicationChannel
    <*> arbitrary -- customerCommunicationCreateContactId :: Text
    <*> arbitraryReducedMaybe n -- customerCommunicationCreateCounterparty :: Maybe Text
    <*> arbitraryReduced n -- customerCommunicationCreateDirection :: CommunicationDirection
    <*> arbitraryReducedMaybe n -- customerCommunicationCreateOccurredAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- customerCommunicationCreateSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerCommunicationCreateTags :: Maybe AnyType
  
instance Arbitrary CustomerCommunicationUpdate where
  arbitrary = sized genCustomerCommunicationUpdate

genCustomerCommunicationUpdate :: Int -> Gen CustomerCommunicationUpdate
genCustomerCommunicationUpdate n =
  CustomerCommunicationUpdate
    <$> arbitraryReducedMaybe n -- customerCommunicationUpdateBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerCommunicationUpdateChannel :: Maybe CommunicationChannel
    <*> arbitraryReducedMaybe n -- customerCommunicationUpdateContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerCommunicationUpdateCounterparty :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerCommunicationUpdateDirection :: Maybe CommunicationDirection
    <*> arbitraryReducedMaybe n -- customerCommunicationUpdateOccurredAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- customerCommunicationUpdateSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerCommunicationUpdateTags :: Maybe AnyType
  
instance Arbitrary CustomerCreate where
  arbitrary = sized genCustomerCreate

genCustomerCreate :: Int -> Gen CustomerCreate
genCustomerCreate n =
  CustomerCreate
    <$> arbitraryReducedMaybe n -- customerCreateAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- customerCreateContactPerson :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerCreateEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerCreateExternalOrderNumber :: Maybe Text
    <*> arbitrary -- customerCreateName :: Text
    <*> arbitraryReducedMaybe n -- customerCreatePaymentGracePeriodDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- customerCreatePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerCreateVatId :: Maybe Text
  
instance Arbitrary CustomerGroup where
  arbitrary = sized genCustomerGroup

genCustomerGroup :: Int -> Gen CustomerGroup
genCustomerGroup n =
  CustomerGroup
    <$> arbitraryReducedMaybe n -- customerGroupDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerGroupMemberIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- customerGroupMembershipFilter :: Maybe Text
    <*> arbitrary -- customerGroupName :: Text
  
instance Arbitrary CustomerGroupCreate where
  arbitrary = sized genCustomerGroupCreate

genCustomerGroupCreate :: Int -> Gen CustomerGroupCreate
genCustomerGroupCreate n =
  CustomerGroupCreate
    <$> arbitraryReducedMaybe n -- customerGroupCreateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerGroupCreateMemberIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- customerGroupCreateMembershipFilter :: Maybe Text
    <*> arbitrary -- customerGroupCreateName :: Text
  
instance Arbitrary CustomerGroupUpdate where
  arbitrary = sized genCustomerGroupUpdate

genCustomerGroupUpdate :: Int -> Gen CustomerGroupUpdate
genCustomerGroupUpdate n =
  CustomerGroupUpdate
    <$> arbitraryReducedMaybe n -- customerGroupUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerGroupUpdateMemberIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- customerGroupUpdateMembershipFilter :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerGroupUpdateName :: Maybe Text
  
instance Arbitrary CustomerInfo where
  arbitrary = sized genCustomerInfo

genCustomerInfo :: Int -> Gen CustomerInfo
genCustomerInfo n =
  CustomerInfo
    <$> arbitrary -- customerInfoAnnualVolume :: Int
    <*> arbitrary -- customerInfoIsRegistered :: Bool
  
instance Arbitrary CustomerUpdate where
  arbitrary = sized genCustomerUpdate

genCustomerUpdate :: Int -> Gen CustomerUpdate
genCustomerUpdate n =
  CustomerUpdate
    <$> arbitraryReducedMaybe n -- customerUpdateAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- customerUpdateContactPerson :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerUpdateEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerUpdateExternalOrderNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerUpdatePaymentGracePeriodDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- customerUpdatePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- customerUpdateVatId :: Maybe Text
  
instance Arbitrary DataQuality where
  arbitrary = sized genDataQuality

genDataQuality :: Int -> Gen DataQuality
genDataQuality n =
  DataQuality
    <$> arbitrary -- dataQualityActivityLines :: Int
    <*> arbitrary -- dataQualityActivitySharePct :: Double
    <*> arbitrary -- dataQualitySpendLines :: Int
  
instance Arbitrary DatevBookingPreview where
  arbitrary = sized genDatevBookingPreview

genDatevBookingPreview :: Int -> Gen DatevBookingPreview
genDatevBookingPreview n =
  DatevBookingPreview
    <$> arbitrary -- datevBookingPreviewAccountNumber :: Text
    <*> arbitrary -- datevBookingPreviewDebitCredit :: Text
    <*> arbitrary -- datevBookingPreviewDocumentDate :: Text
    <*> arbitrary -- datevBookingPreviewDocumentText :: Text
    <*> arbitrary -- datevBookingPreviewNetAmount :: Text
    <*> arbitrary -- datevBookingPreviewOppositeAccount :: Text
    <*> arbitraryReducedMaybe n -- datevBookingPreviewTaxAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- datevBookingPreviewTaxRate :: Maybe Text
  
instance Arbitrary DatevExportResponse where
  arbitrary = sized genDatevExportResponse

genDatevExportResponse :: Int -> Gen DatevExportResponse
genDatevExportResponse n =
  DatevExportResponse
    <$> arbitrary -- datevExportResponseBookingCount :: Int
    <*> arbitrary -- datevExportResponseCsvContent :: Text
    <*> arbitrary -- datevExportResponseFilename :: Text
  
instance Arbitrary DatevImportResponse where
  arbitrary = sized genDatevImportResponse

genDatevImportResponse :: Int -> Gen DatevImportResponse
genDatevImportResponse n =
  DatevImportResponse
    <$> arbitrary -- datevImportResponseCount :: Int
    <*> arbitrary -- datevImportResponseFilename :: Text
    <*> arbitraryReduced n -- datevImportResponseRows :: [DatevImportRow]
  
instance Arbitrary DatevImportRow where
  arbitrary = sized genDatevImportRow

genDatevImportRow :: Int -> Gen DatevImportRow
genDatevImportRow n =
  DatevImportRow
    <$> arbitrary -- datevImportRowAccount :: Text
    <*> arbitrary -- datevImportRowAmount :: Text
    <*> arbitrary -- datevImportRowBaseAmount :: Text
    <*> arbitrary -- datevImportRowBaseCurrency :: Text
    <*> arbitrary -- datevImportRowBookingText :: Text
    <*> arbitrary -- datevImportRowBuKey :: Text
    <*> arbitrary -- datevImportRowCostCenter1 :: Text
    <*> arbitrary -- datevImportRowCostCenter2 :: Text
    <*> arbitrary -- datevImportRowCurrency :: Text
    <*> arbitrary -- datevImportRowDebitCredit :: Text
    <*> arbitrary -- datevImportRowDiscount :: Text
    <*> arbitrary -- datevImportRowDocumentDate :: Text
    <*> arbitrary -- datevImportRowDocumentField2 :: Text
    <*> arbitrary -- datevImportRowDocumentNumber :: Text
    <*> arbitrary -- datevImportRowEuCountryVatId :: Text
    <*> arbitrary -- datevImportRowEuTaxRate :: Text
    <*> arbitrary -- datevImportRowExchangeRate :: Text
    <*> arbitrary -- datevImportRowOppositeAccount :: Text
  
instance Arbitrary Declaration where
  arbitrary = sized genDeclaration

genDeclaration :: Int -> Gen Declaration
genDeclaration n =
  Declaration
    <$> arbitraryReducedMaybe n -- declarationDeclarationType :: Maybe DeclarationType
    <*> arbitraryReducedMaybe n -- declarationIsCurrent :: Maybe Bool
    <*> arbitraryReducedMaybe n -- declarationText :: Maybe Text
    <*> arbitraryReducedMaybe n -- declarationValidFrom :: Maybe Date
    <*> arbitraryReducedMaybe n -- declarationVersion :: Maybe Text
  
instance Arbitrary DeclarationCreate where
  arbitrary = sized genDeclarationCreate

genDeclarationCreate :: Int -> Gen DeclarationCreate
genDeclarationCreate n =
  DeclarationCreate
    <$> arbitraryReducedMaybe n -- declarationCreateDeclarationType :: Maybe DeclarationType
    <*> arbitraryReducedMaybe n -- declarationCreateIsCurrent :: Maybe Bool
    <*> arbitraryReducedMaybe n -- declarationCreateText :: Maybe Text
    <*> arbitraryReducedMaybe n -- declarationCreateValidFrom :: Maybe Date
    <*> arbitraryReducedMaybe n -- declarationCreateVersion :: Maybe Text
  
instance Arbitrary DeclarationUpdate where
  arbitrary = sized genDeclarationUpdate

genDeclarationUpdate :: Int -> Gen DeclarationUpdate
genDeclarationUpdate n =
  DeclarationUpdate
    <$> arbitraryReducedMaybe n -- declarationUpdateDeclarationType :: Maybe DeclarationType
    <*> arbitraryReducedMaybe n -- declarationUpdateIsCurrent :: Maybe Bool
    <*> arbitraryReducedMaybe n -- declarationUpdateText :: Maybe Text
    <*> arbitraryReducedMaybe n -- declarationUpdateValidFrom :: Maybe Date
    <*> arbitraryReducedMaybe n -- declarationUpdateVersion :: Maybe Text
  
instance Arbitrary DeliverableResponse where
  arbitrary = sized genDeliverableResponse

genDeliverableResponse :: Int -> Gen DeliverableResponse
genDeliverableResponse n =
  DeliverableResponse
    <$> arbitrary -- deliverableResponseAvailableStock :: Integer
    <*> arbitrary -- deliverableResponseDeliverableQuantity :: Integer
    <*> arbitraryReducedMaybe n -- deliverableResponseMaxSellable :: Maybe Integer
    <*> arbitrary -- deliverableResponseProductId :: Text
    <*> arbitrary -- deliverableResponseReservedStock :: Integer
    <*> arbitraryReducedMaybe n -- deliverableResponseWarehouseId :: Maybe Text
  
instance Arbitrary DeliveryAppointment where
  arbitrary = sized genDeliveryAppointment

genDeliveryAppointment :: Int -> Gen DeliveryAppointment
genDeliveryAppointment n =
  DeliveryAppointment
    <$> arbitrary -- deliveryAppointmentEmail :: Text
    <*> arbitraryReducedMaybe n -- deliveryAppointmentNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryAppointmentPhone :: Maybe Text
    <*> arbitraryReduced n -- deliveryAppointmentRequestedDate :: Date
    <*> arbitraryReduced n -- deliveryAppointmentStatus :: DeliveryAppointmentStatus
    <*> arbitrary -- deliveryAppointmentSupplierName :: Text
    <*> arbitraryReducedMaybe n -- deliveryAppointmentTimeSlot :: Maybe Text
    <*> arbitrary -- deliveryAppointmentWarehouseId :: Text
  
instance Arbitrary DeliveryAppointmentCreate where
  arbitrary = sized genDeliveryAppointmentCreate

genDeliveryAppointmentCreate :: Int -> Gen DeliveryAppointmentCreate
genDeliveryAppointmentCreate n =
  DeliveryAppointmentCreate
    <$> arbitrary -- deliveryAppointmentCreateEmail :: Text
    <*> arbitraryReducedMaybe n -- deliveryAppointmentCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryAppointmentCreatePhone :: Maybe Text
    <*> arbitraryReduced n -- deliveryAppointmentCreateRequestedDate :: Date
    <*> arbitraryReduced n -- deliveryAppointmentCreateStatus :: DeliveryAppointmentStatus
    <*> arbitrary -- deliveryAppointmentCreateSupplierName :: Text
    <*> arbitraryReducedMaybe n -- deliveryAppointmentCreateTimeSlot :: Maybe Text
    <*> arbitrary -- deliveryAppointmentCreateWarehouseId :: Text
  
instance Arbitrary DeliveryDate where
  arbitrary = sized genDeliveryDate

genDeliveryDate :: Int -> Gen DeliveryDate
genDeliveryDate n =
  DeliveryDate
    <$> arbitraryReducedMaybe n -- deliveryDateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryDateFulfilledDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryDateNote :: Maybe Text
    <*> arbitrary -- deliveryDateOrderNumber :: Text
    <*> arbitraryReducedMaybe n -- deliveryDateOriginalDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryDateProductId :: Maybe Text
    <*> arbitraryReduced n -- deliveryDatePromisedDate :: Date
    <*> arbitraryReduced n -- deliveryDateStatus :: DeliveryDateStatus
  
instance Arbitrary DeliveryDateCreate where
  arbitrary = sized genDeliveryDateCreate

genDeliveryDateCreate :: Int -> Gen DeliveryDateCreate
genDeliveryDateCreate n =
  DeliveryDateCreate
    <$> arbitraryReducedMaybe n -- deliveryDateCreateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryDateCreateFulfilledDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryDateCreateNote :: Maybe Text
    <*> arbitrary -- deliveryDateCreateOrderNumber :: Text
    <*> arbitraryReducedMaybe n -- deliveryDateCreateOriginalDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryDateCreateProductId :: Maybe Text
    <*> arbitraryReduced n -- deliveryDateCreatePromisedDate :: Date
    <*> arbitraryReduced n -- deliveryDateCreateStatus :: DeliveryDateStatus
  
instance Arbitrary DeliveryDateStatusUpdate where
  arbitrary = sized genDeliveryDateStatusUpdate

genDeliveryDateStatusUpdate :: Int -> Gen DeliveryDateStatusUpdate
genDeliveryDateStatusUpdate n =
  DeliveryDateStatusUpdate
    <$> arbitrary -- deliveryDateStatusUpdateStatus :: Text
  
instance Arbitrary DeliveryDateUpdate where
  arbitrary = sized genDeliveryDateUpdate

genDeliveryDateUpdate :: Int -> Gen DeliveryDateUpdate
genDeliveryDateUpdate n =
  DeliveryDateUpdate
    <$> arbitraryReducedMaybe n -- deliveryDateUpdateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryDateUpdateFulfilledDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryDateUpdateNote :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryDateUpdateOrderNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryDateUpdateOriginalDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryDateUpdateProductId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryDateUpdatePromisedDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryDateUpdateStatus :: Maybe DeliveryDateStatus
  
instance Arbitrary DeliveryNote where
  arbitrary = sized genDeliveryNote

genDeliveryNote :: Int -> Gen DeliveryNote
genDeliveryNote n =
  DeliveryNote
    <$> arbitraryReducedMaybe n -- deliveryNoteAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- deliveryNoteContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteContactName :: Maybe Text
    <*> arbitrary -- deliveryNoteCurrency :: Text
    <*> arbitraryReducedMaybe n -- deliveryNoteDeliveryDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryNoteDeliveryNoteNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteFiles :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- deliveryNoteIntroduction :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- deliveryNotePrecedingSalesVoucherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNotePrecedingSalesVoucherType :: Maybe PrecedingSalesVoucherType
    <*> arbitraryReducedMaybe n -- deliveryNoteRemark :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteShippingDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryNoteShippingMethod :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteSubtotal :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteTotalAmount :: Maybe Text
    <*> arbitraryReduced n -- deliveryNoteVoucherDate :: Date
    <*> arbitraryReduced n -- deliveryNoteVoucherStatus :: VoucherStatus
  
instance Arbitrary DeliveryNoteCreate where
  arbitrary = sized genDeliveryNoteCreate

genDeliveryNoteCreate :: Int -> Gen DeliveryNoteCreate
genDeliveryNoteCreate n =
  DeliveryNoteCreate
    <$> arbitraryReducedMaybe n -- deliveryNoteCreateAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateContactName :: Maybe Text
    <*> arbitrary -- deliveryNoteCreateCurrency :: Text
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateDeliveryDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateDeliveryNoteNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateFiles :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateIntroduction :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- deliveryNoteCreatePrecedingSalesVoucherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteCreatePrecedingSalesVoucherType :: Maybe PrecedingSalesVoucherType
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateRemark :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateShippingDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateShippingMethod :: Maybe Text
    <*> arbitraryReducedMaybe n -- deliveryNoteCreateTitle :: Maybe Text
    <*> arbitraryReduced n -- deliveryNoteCreateVoucherDate :: Date
    <*> arbitraryReduced n -- deliveryNoteCreateVoucherStatus :: VoucherStatus
  
instance Arbitrary DhlCredentials where
  arbitrary = sized genDhlCredentials

genDhlCredentials :: Int -> Gen DhlCredentials
genDhlCredentials n =
  DhlCredentials
    <$> arbitrary -- dhlCredentialsApiKey :: Text
    <*> arbitraryReducedMaybe n -- dhlCredentialsClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- dhlCredentialsClientSecret :: Maybe Text
  
instance Arbitrary DownPaymentInvoice where
  arbitrary = sized genDownPaymentInvoice

genDownPaymentInvoice :: Int -> Gen DownPaymentInvoice
genDownPaymentInvoice n =
  DownPaymentInvoice
    <$> arbitraryReducedMaybe n -- downPaymentInvoiceContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- downPaymentInvoiceContactName :: Maybe Text
    <*> arbitrary -- downPaymentInvoiceCreatedAt :: Text
    <*> arbitrary -- downPaymentInvoiceCurrency :: Text
    <*> arbitrary -- downPaymentInvoiceId :: Text
    <*> arbitraryReducedMaybe n -- downPaymentInvoiceNotes :: Maybe Text
    <*> arbitrary -- downPaymentInvoicePaidAmount :: Text
    <*> arbitrary -- downPaymentInvoiceTotalAmount :: Text
    <*> arbitraryReduced n -- downPaymentInvoiceVoucherDate :: Date
    <*> arbitraryReducedMaybe n -- downPaymentInvoiceVoucherNumber :: Maybe Text
    <*> arbitrary -- downPaymentInvoiceVoucherStatus :: Text
  
instance Arbitrary DpaAcceptRequest where
  arbitrary = sized genDpaAcceptRequest

genDpaAcceptRequest :: Int -> Gen DpaAcceptRequest
genDpaAcceptRequest n =
  DpaAcceptRequest
    <$> arbitrary -- dpaAcceptRequestAcceptedByName :: Text
    <*> arbitrary -- dpaAcceptRequestVersion :: Text
  
instance Arbitrary DpaStatus where
  arbitrary = sized genDpaStatus

genDpaStatus :: Int -> Gen DpaStatus
genDpaStatus n =
  DpaStatus
    <$> arbitrary -- dpaStatusAccepted :: Bool
    <*> arbitraryReducedMaybe n -- dpaStatusAcceptedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- dpaStatusAcceptedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- dpaStatusVersion :: Maybe Text
  
instance Arbitrary DunningResult where
  arbitrary = sized genDunningResult

genDunningResult :: Int -> Gen DunningResult
genDunningResult n =
  DunningResult
    <$> arbitrary -- dunningResultInvoicesProcessed :: Int
    <*> arbitrary -- dunningResultMessage :: Text
  
instance Arbitrary EBilanzReport where
  arbitrary = sized genEBilanzReport

genEBilanzReport :: Int -> Gen EBilanzReport
genEBilanzReport n =
  EBilanzReport
    <$> arbitraryReduced n -- eBilanzReportAccountOverview :: [AccountOverview]
    <*> arbitraryReduced n -- eBilanzReportBalanceSheet :: BalanceSheet
    <*> arbitrary -- eBilanzReportGeneratedAt :: Text
    <*> arbitraryReduced n -- eBilanzReportIncomeStatement :: IncomeStatement
    <*> arbitrary -- eBilanzReportPeriod :: Text
    <*> arbitraryReduced n -- eBilanzReportVatSummary :: VatSummary
  
instance Arbitrary EksErgebnis where
  arbitrary = sized genEksErgebnis

genEksErgebnis :: Int -> Gen EksErgebnis
genEksErgebnis n =
  EksErgebnis
    <$> arbitrary -- eksErgebnisGesamtergebnis :: Text
    <*> arbitraryReduced n -- eksErgebnisMonate :: [EksMonatsWert]
    <*> arbitrary -- eksErgebnisPrognoseNaechste6Monate :: Text
    <*> arbitrary -- eksErgebnisSummeAusgaben :: Text
    <*> arbitrary -- eksErgebnisSummeEinnahmen :: Text
    <*> arbitrary -- eksErgebnisZeitraumBis :: Text
    <*> arbitrary -- eksErgebnisZeitraumVon :: Text
  
instance Arbitrary EksMonatsWert where
  arbitrary = sized genEksMonatsWert

genEksMonatsWert :: Int -> Gen EksMonatsWert
genEksMonatsWert n =
  EksMonatsWert
    <$> arbitrary -- eksMonatsWertAusgaben :: Text
    <*> arbitrary -- eksMonatsWertEinnahmen :: Text
    <*> arbitrary -- eksMonatsWertErgebnis :: Text
    <*> arbitrary -- eksMonatsWertMonat :: Text
  
instance Arbitrary ElsterStatus where
  arbitrary = sized genElsterStatus

genElsterStatus :: Int -> Gen ElsterStatus
genElsterStatus n =
  ElsterStatus
    <$> arbitrary -- elsterStatusCertConfigured :: Bool
    <*> arbitrary -- elsterStatusEricAvailable :: Bool
    <*> arbitraryReducedMaybe n -- elsterStatusEricVersion :: Maybe Text
    <*> arbitrary -- elsterStatusFeatureEnabled :: Bool
    <*> arbitrary -- elsterStatusHint :: Text
    <*> arbitrary -- elsterStatusMode :: Text
    <*> arbitrary -- elsterStatusVendorIdConfigured :: Bool
  
instance Arbitrary EmailTemplate where
  arbitrary = sized genEmailTemplate

genEmailTemplate :: Int -> Gen EmailTemplate
genEmailTemplate n =
  EmailTemplate
    <$> arbitrary -- emailTemplateBody :: Text
    <*> arbitrary -- emailTemplateName :: Text
    <*> arbitraryReduced n -- emailTemplateStatus :: EmailTemplateStatus
    <*> arbitrary -- emailTemplateSubject :: Text
    <*> arbitraryReducedMaybe n -- emailTemplateVariables :: Maybe AnyType
  
instance Arbitrary EmailTemplateCreate where
  arbitrary = sized genEmailTemplateCreate

genEmailTemplateCreate :: Int -> Gen EmailTemplateCreate
genEmailTemplateCreate n =
  EmailTemplateCreate
    <$> arbitrary -- emailTemplateCreateBody :: Text
    <*> arbitrary -- emailTemplateCreateName :: Text
    <*> arbitraryReduced n -- emailTemplateCreateStatus :: EmailTemplateStatus
    <*> arbitrary -- emailTemplateCreateSubject :: Text
    <*> arbitraryReducedMaybe n -- emailTemplateCreateVariables :: Maybe AnyType
  
instance Arbitrary EmailTemplateUpdate where
  arbitrary = sized genEmailTemplateUpdate

genEmailTemplateUpdate :: Int -> Gen EmailTemplateUpdate
genEmailTemplateUpdate n =
  EmailTemplateUpdate
    <$> arbitraryReducedMaybe n -- emailTemplateUpdateBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateUpdateStatus :: Maybe EmailTemplateStatus
    <*> arbitraryReducedMaybe n -- emailTemplateUpdateSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateUpdateVariables :: Maybe AnyType
  
instance Arbitrary EmissionEntry where
  arbitrary = sized genEmissionEntry

genEmissionEntry :: Int -> Gen EmissionEntry
genEmissionEntry n =
  EmissionEntry
    <$> arbitrary -- emissionEntryActivityValue :: Text
    <*> arbitrary -- emissionEntryCategoryId :: Text
    <*> arbitrary -- emissionEntryDescription :: Text
    <*> arbitrary -- emissionEntryEfSource :: Text
    <*> arbitrary -- emissionEntryEfVersion :: Text
    <*> arbitraryReduced n -- emissionEntryMethod :: EmissionMethod
    <*> arbitraryReduced n -- emissionEntryScope :: GhgScope
    <*> arbitrary -- emissionEntryTco2e :: Text
    <*> arbitrary -- emissionEntryUnit :: Text
    <*> arbitraryReducedMaybe n -- emissionEntryUpdatedAt :: Maybe DateTime
    <*> arbitrary -- emissionEntryYear :: Int
  
instance Arbitrary EmissionFactorResponse where
  arbitrary = sized genEmissionFactorResponse

genEmissionFactorResponse :: Int -> Gen EmissionFactorResponse
genEmissionFactorResponse n =
  EmissionFactorResponse
    <$> arbitrary -- emissionFactorResponseCategoryId :: Text
    <*> arbitrary -- emissionFactorResponseKgCo2ePerUnit :: Double
    <*> arbitrary -- emissionFactorResponseNameDe :: Text
    <*> arbitrary -- emissionFactorResponseSource :: Text
    <*> arbitrary -- emissionFactorResponseUnit :: Text
    <*> arbitrary -- emissionFactorResponseVersion :: Text
  
instance Arbitrary EmissionTarget where
  arbitrary = sized genEmissionTarget

genEmissionTarget :: Int -> Gen EmissionTarget
genEmissionTarget n =
  EmissionTarget
    <$> arbitrary -- emissionTargetBaseValue :: Text
    <*> arbitrary -- emissionTargetBaseYear :: Int
    <*> arbitrary -- emissionTargetDescription :: Text
    <*> arbitraryReduced n -- emissionTargetScope :: EmissionTargetScope
    <*> arbitrary -- emissionTargetTargetValue :: Text
    <*> arbitrary -- emissionTargetTargetYear :: Int
    <*> arbitraryReducedMaybe n -- emissionTargetUpdatedAt :: Maybe DateTime
  
instance Arbitrary EmissionsExportResponse where
  arbitrary = sized genEmissionsExportResponse

genEmissionsExportResponse :: Int -> Gen EmissionsExportResponse
genEmissionsExportResponse n =
  EmissionsExportResponse
    <$> arbitrary -- emissionsExportResponseCsvContent :: Text
    <*> arbitrary -- emissionsExportResponseFilename :: Text
  
instance Arbitrary EmissionsReport where
  arbitrary = sized genEmissionsReport

genEmissionsReport :: Int -> Gen EmissionsReport
genEmissionsReport n =
  EmissionsReport
    <$> arbitraryReduced n -- emissionsReportByCategory :: [CategoryTotal]
    <*> arbitraryReduced n -- emissionsReportByScope :: [ScopeTotal]
    <*> arbitraryReduced n -- emissionsReportByYear :: [YearTotal]
    <*> arbitraryReduced n -- emissionsReportDataQuality :: DataQuality
    <*> arbitraryReducedMaybe n -- emissionsReportIntensityPerEmployee :: Maybe Double
    <*> arbitraryReducedMaybe n -- emissionsReportIntensityPerRevenueMio :: Maybe Double
    <*> arbitraryReducedMaybe n -- emissionsReportNetRevenue :: Maybe Double
    <*> arbitraryReducedMaybe n -- emissionsReportSpendBasedEstimateTco2e :: Maybe Double
    <*> arbitraryReduced n -- emissionsReportTargets :: [TargetProgress]
    <*> arbitrary -- emissionsReportTotalTco2e :: Text
  
instance Arbitrary EmitEventRequest where
  arbitrary = sized genEmitEventRequest

genEmitEventRequest :: Int -> Gen EmitEventRequest
genEmitEventRequest n =
  EmitEventRequest
    <$> arbitrary -- emitEventRequestEventType :: Text
    <*> arbitraryReducedMaybe n -- emitEventRequestPayload :: Maybe AnyType
  
instance Arbitrary Employee where
  arbitrary = sized genEmployee

genEmployee :: Int -> Gen Employee
genEmployee n =
  Employee
    <$> arbitraryReducedMaybe n -- employeeAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeBackupEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeBic :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- employeeCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- employeeDateOfBirth :: Maybe Date
    <*> arbitraryReducedMaybe n -- employeeDeletedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- employeeDepartmentId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeGender :: Maybe Gender
    <*> arbitraryReducedMaybe n -- employeeHireDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- employeeHourlyCost :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeIban :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeJobTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeLastLogin :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- employeeLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- employeeMonthlySalary :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeState :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeStatus :: Maybe EmployeeStatus
    <*> arbitraryReducedMaybe n -- employeeTenantId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- employeeUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeWeeklyHours :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeZip :: Maybe Text
  
instance Arbitrary EmployeeCreate where
  arbitrary = sized genEmployeeCreate

genEmployeeCreate :: Int -> Gen EmployeeCreate
genEmployeeCreate n =
  EmployeeCreate
    <$> arbitraryReducedMaybe n -- employeeCreateAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateBackupEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateBic :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- employeeCreateDateOfBirth :: Maybe Date
    <*> arbitraryReducedMaybe n -- employeeCreateDepartmentId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateGender :: Maybe Gender
    <*> arbitraryReducedMaybe n -- employeeCreateHireDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- employeeCreateHourlyCost :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateIban :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateJobTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateLastLogin :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- employeeCreateLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- employeeCreateMonthlySalary :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreatePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateState :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateStatus :: Maybe EmployeeStatus
    <*> arbitraryReducedMaybe n -- employeeCreateUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateWeeklyHours :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeCreateZip :: Maybe Text
  
instance Arbitrary EmployeeUpdate where
  arbitrary = sized genEmployeeUpdate

genEmployeeUpdate :: Int -> Gen EmployeeUpdate
genEmployeeUpdate n =
  EmployeeUpdate
    <$> arbitraryReducedMaybe n -- employeeUpdateAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateBackupEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateBic :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- employeeUpdateDateOfBirth :: Maybe Date
    <*> arbitraryReducedMaybe n -- employeeUpdateDepartmentId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateGender :: Maybe Gender
    <*> arbitraryReducedMaybe n -- employeeUpdateHireDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- employeeUpdateHourlyCost :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateIban :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateJobTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateLastLogin :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- employeeUpdateLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- employeeUpdateMonthlySalary :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdatePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateState :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateStatus :: Maybe EmployeeStatus
    <*> arbitraryReducedMaybe n -- employeeUpdateUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateWeeklyHours :: Maybe Text
    <*> arbitraryReducedMaybe n -- employeeUpdateZip :: Maybe Text
  
instance Arbitrary EuerDetailErgebnis where
  arbitrary = sized genEuerDetailErgebnis

genEuerDetailErgebnis :: Int -> Gen EuerDetailErgebnis
genEuerDetailErgebnis n =
  EuerDetailErgebnis
    <$> arbitrary -- euerDetailErgebnisJahr :: Int
    <*> arbitraryReduced n -- euerDetailErgebnisZeilen :: [EuerZeileDetail]
  
instance Arbitrary EuerErgebnis where
  arbitrary = sized genEuerErgebnis

genEuerErgebnis :: Int -> Gen EuerErgebnis
genEuerErgebnis n =
  EuerErgebnis
    <$> arbitrary -- euerErgebnisAnlageZugaenge :: Text
    <*> arbitrary -- euerErgebnisGewinnVerlust :: Text
    <*> arbitrary -- euerErgebnisJahr :: Int
    <*> arbitrary -- euerErgebnisSummeAusgaben :: Text
    <*> arbitrary -- euerErgebnisSummeEinnahmen :: Text
    <*> arbitraryReduced n -- euerErgebnisZeilen :: [EuerZeile]
  
instance Arbitrary EuerKatSumme where
  arbitrary = sized genEuerKatSumme

genEuerKatSumme :: Int -> Gen EuerKatSumme
genEuerKatSumme n =
  EuerKatSumme
    <$> arbitrary -- euerKatSummeBetrag :: Text
    <*> arbitrary -- euerKatSummeName :: Text
  
instance Arbitrary EuerZeile where
  arbitrary = sized genEuerZeile

genEuerZeile :: Int -> Gen EuerZeile
genEuerZeile n =
  EuerZeile
    <$> arbitrary -- euerZeileAbschnitt :: Text
    <*> arbitrary -- euerZeileBetrag :: Text
    <*> arbitrary -- euerZeileBezeichnung :: Text
    <*> arbitrary -- euerZeileZeile :: Int
  
instance Arbitrary EuerZeileDetail where
  arbitrary = sized genEuerZeileDetail

genEuerZeileDetail :: Int -> Gen EuerZeileDetail
genEuerZeileDetail n =
  EuerZeileDetail
    <$> arbitrary -- euerZeileDetailAbschnitt :: Text
    <*> arbitrary -- euerZeileDetailBetragGesamt :: Text
    <*> arbitrary -- euerZeileDetailBezeichnung :: Text
    <*> arbitraryReduced n -- euerZeileDetailKategorien :: [EuerKatSumme]
    <*> arbitrary -- euerZeileDetailZeile :: Int
  
instance Arbitrary EventSubscription where
  arbitrary = sized genEventSubscription

genEventSubscription :: Int -> Gen EventSubscription
genEventSubscription n =
  EventSubscription
    <$> arbitrary -- eventSubscriptionCallbackUrl :: Text
    <*> arbitrary -- eventSubscriptionEventType :: Text
    <*> arbitrary -- eventSubscriptionIsActive :: Bool
    <*> arbitrary -- eventSubscriptionSubscriptionId :: Text
  
instance Arbitrary ExpenseItem where
  arbitrary = sized genExpenseItem

genExpenseItem :: Int -> Gen ExpenseItem
genExpenseItem n =
  ExpenseItem
    <$> arbitrary -- expenseItemAmount :: Text
    <*> arbitrary -- expenseItemCategory :: Text
    <*> arbitrary -- expenseItemPercentage :: Double
  
instance Arbitrary ExtraPayment where
  arbitrary = sized genExtraPayment

genExtraPayment :: Int -> Gen ExtraPayment
genExtraPayment n =
  ExtraPayment
    <$> arbitrary -- extraPaymentAmount :: Text
    <*> arbitrary -- extraPaymentEmployeeId :: Text
    <*> arbitraryReducedMaybe n -- extraPaymentReason :: Maybe Text
  
instance Arbitrary FeatureSettings where
  arbitrary = sized genFeatureSettings

genFeatureSettings :: Int -> Gen FeatureSettings
genFeatureSettings n =
  FeatureSettings
    <$> arbitrary -- featureSettingsOnlineshop :: Bool
    <*> arbitrary -- featureSettingsReportBilanz :: Bool
    <*> arbitrary -- featureSettingsReportBwa :: Bool
    <*> arbitrary -- featureSettingsReportEuer :: Bool
    <*> arbitrary -- featureSettingsReportGewerbesteuer :: Bool
    <*> arbitrary -- featureSettingsReportGuv :: Bool
    <*> arbitrary -- featureSettingsReportKst :: Bool
    <*> arbitrary -- featureSettingsReportUstva :: Bool
  
instance Arbitrary ForgotPasswordRequest where
  arbitrary = sized genForgotPasswordRequest

genForgotPasswordRequest :: Int -> Gen ForgotPasswordRequest
genForgotPasswordRequest n =
  ForgotPasswordRequest
    <$> arbitrary -- forgotPasswordRequestEmail :: Text
  
instance Arbitrary FristEintrag where
  arbitrary = sized genFristEintrag

genFristEintrag :: Int -> Gen FristEintrag
genFristEintrag n =
  FristEintrag
    <$> arbitrary -- fristEintragBezeichnung :: Text
    <*> arbitrary -- fristEintragFaellig :: Text
    <*> arbitrary -- fristEintragFaelligOriginal :: Text
    <*> arbitraryReducedMaybe n -- fristEintragHinweis :: Maybe Text
    <*> arbitrary -- fristEintragTyp :: Text
    <*> arbitrary -- fristEintragZeitraum :: Text
  
instance Arbitrary FristenErgebnis where
  arbitrary = sized genFristenErgebnis

genFristenErgebnis :: Int -> Gen FristenErgebnis
genFristenErgebnis n =
  FristenErgebnis
    <$> arbitrary -- fristenErgebnisAnzahl :: Int
    <*> arbitraryReduced n -- fristenErgebnisFristen :: [FristEintrag]
  
instance Arbitrary GatewayOAuthAuthorizeRequest where
  arbitrary = sized genGatewayOAuthAuthorizeRequest

genGatewayOAuthAuthorizeRequest :: Int -> Gen GatewayOAuthAuthorizeRequest
genGatewayOAuthAuthorizeRequest n =
  GatewayOAuthAuthorizeRequest
    <$> arbitrary -- gatewayOAuthAuthorizeRequestGatewayType :: Text
    <*> arbitrary -- gatewayOAuthAuthorizeRequestRedirectUri :: Text
  
instance Arbitrary GatewayOAuthAuthorizeResponse where
  arbitrary = sized genGatewayOAuthAuthorizeResponse

genGatewayOAuthAuthorizeResponse :: Int -> Gen GatewayOAuthAuthorizeResponse
genGatewayOAuthAuthorizeResponse n =
  GatewayOAuthAuthorizeResponse
    <$> arbitrary -- gatewayOAuthAuthorizeResponseAuthorizationUrl :: Text
    <*> arbitrary -- gatewayOAuthAuthorizeResponseState :: Text
  
instance Arbitrary GatewayOAuthCallbackRequest where
  arbitrary = sized genGatewayOAuthCallbackRequest

genGatewayOAuthCallbackRequest :: Int -> Gen GatewayOAuthCallbackRequest
genGatewayOAuthCallbackRequest n =
  GatewayOAuthCallbackRequest
    <$> arbitrary -- gatewayOAuthCallbackRequestCode :: Text
    <*> arbitrary -- gatewayOAuthCallbackRequestGatewayType :: Text
    <*> arbitrary -- gatewayOAuthCallbackRequestRedirectUri :: Text
    <*> arbitrary -- gatewayOAuthCallbackRequestState :: Text
  
instance Arbitrary GdprActivity where
  arbitrary = sized genGdprActivity

genGdprActivity :: Int -> Gen GdprActivity
genGdprActivity n =
  GdprActivity
    <$> arbitrary -- gdprActivityAction :: Text
    <*> arbitraryReduced n -- gdprActivityCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- gdprActivityDescription :: Maybe Text
    <*> arbitrary -- gdprActivityId :: Text
    <*> arbitrary -- gdprActivityTenantId :: Text
  
instance Arbitrary GdprApiKey where
  arbitrary = sized genGdprApiKey

genGdprApiKey :: Int -> Gen GdprApiKey
genGdprApiKey n =
  GdprApiKey
    <$> arbitraryReduced n -- gdprApiKeyCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- gdprApiKeyExpiresAt :: Maybe DateTime
    <*> arbitrary -- gdprApiKeyId :: Text
    <*> arbitrary -- gdprApiKeyKeyId :: Text
    <*> arbitrary -- gdprApiKeyName :: Text
    <*> arbitrary -- gdprApiKeyRevoked :: Bool
  
instance Arbitrary GdprBillingInfo where
  arbitrary = sized genGdprBillingInfo

genGdprBillingInfo :: Int -> Gen GdprBillingInfo
genGdprBillingInfo n =
  GdprBillingInfo
    <$> arbitraryReducedMaybe n -- gdprBillingInfoCurrentPeriodEnd :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- gdprBillingInfoCurrentPeriodStart :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- gdprBillingInfoPlan :: Maybe Text
    <*> arbitraryReducedMaybe n -- gdprBillingInfoStatus :: Maybe Text
    <*> arbitrary -- gdprBillingInfoTenantId :: Text
  
instance Arbitrary GdprExport where
  arbitrary = sized genGdprExport

genGdprExport :: Int -> Gen GdprExport
genGdprExport n =
  GdprExport
    <$> arbitraryReduced n -- gdprExportActivityLog :: [GdprActivity]
    <*> arbitraryReduced n -- gdprExportApiKeys :: [GdprApiKey]
    <*> arbitraryReduced n -- gdprExportBilling :: [GdprBillingInfo]
    <*> arbitraryReduced n -- gdprExportExportedAt :: DateTime
    <*> arbitrary -- gdprExportGeneratedByAi :: Bool
    <*> arbitraryReduced n -- gdprExportNotifications :: [GdprNotification]
    <*> arbitraryReduced n -- gdprExportRefreshTokens :: [GdprRefreshToken]
    <*> arbitraryReduced n -- gdprExportTenants :: [GdprTenant]
    <*> arbitraryReduced n -- gdprExportUsageEvents :: [GdprUsageEvent]
    <*> arbitraryReduced n -- gdprExportUser :: GdprUser
  
instance Arbitrary GdprNotification where
  arbitrary = sized genGdprNotification

genGdprNotification :: Int -> Gen GdprNotification
genGdprNotification n =
  GdprNotification
    <$> arbitraryReduced n -- gdprNotificationCreatedAt :: DateTime
    <*> arbitrary -- gdprNotificationId :: Text
    <*> arbitrary -- gdprNotificationIsRead :: Bool
    <*> arbitraryReducedMaybe n -- gdprNotificationMessage :: Maybe Text
    <*> arbitrary -- gdprNotificationTenantId :: Text
    <*> arbitrary -- gdprNotificationTitle :: Text
  
instance Arbitrary GdprRefreshToken where
  arbitrary = sized genGdprRefreshToken

genGdprRefreshToken :: Int -> Gen GdprRefreshToken
genGdprRefreshToken n =
  GdprRefreshToken
    <$> arbitraryReduced n -- gdprRefreshTokenCreatedAt :: DateTime
    <*> arbitraryReduced n -- gdprRefreshTokenExpiresAt :: DateTime
    <*> arbitrary -- gdprRefreshTokenId :: Text
    <*> arbitraryReducedMaybe n -- gdprRefreshTokenRevokedAt :: Maybe DateTime
    <*> arbitrary -- gdprRefreshTokenTenantId :: Text
  
instance Arbitrary GdprTenant where
  arbitrary = sized genGdprTenant

genGdprTenant :: Int -> Gen GdprTenant
genGdprTenant n =
  GdprTenant
    <$> arbitrary -- gdprTenantName :: Text
    <*> arbitrary -- gdprTenantRole :: Text
    <*> arbitrary -- gdprTenantTenantId :: Text
  
instance Arbitrary GdprUsageEvent where
  arbitrary = sized genGdprUsageEvent

genGdprUsageEvent :: Int -> Gen GdprUsageEvent
genGdprUsageEvent n =
  GdprUsageEvent
    <$> arbitraryReduced n -- gdprUsageEventCreatedAt :: DateTime
    <*> arbitrary -- gdprUsageEventEventType :: Text
    <*> arbitrary -- gdprUsageEventId :: Text
    <*> arbitrary -- gdprUsageEventQuantity :: Int
    <*> arbitrary -- gdprUsageEventTenantId :: Text
  
instance Arbitrary GdprUser where
  arbitrary = sized genGdprUser

genGdprUser :: Int -> Gen GdprUser
genGdprUser n =
  GdprUser
    <$> arbitraryReduced n -- gdprUserCreatedAt :: DateTime
    <*> arbitrary -- gdprUserEmail :: Text
    <*> arbitrary -- gdprUserId :: Text
    <*> arbitrary -- gdprUserName :: Text
  
instance Arbitrary GenerateCountRequest where
  arbitrary = sized genGenerateCountRequest

genGenerateCountRequest :: Int -> Gen GenerateCountRequest
genGenerateCountRequest n =
  GenerateCountRequest
    <$> arbitraryReducedMaybe n -- generateCountRequestNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- generateCountRequestProductIds :: Maybe [Text]
    <*> arbitrary -- generateCountRequestWarehouseId :: Text
  
instance Arbitrary GenerateVariantsRequest where
  arbitrary = sized genGenerateVariantsRequest

genGenerateVariantsRequest :: Int -> Gen GenerateVariantsRequest
genGenerateVariantsRequest n =
  GenerateVariantsRequest
    <$> arbitraryReducedMaybe n -- generateVariantsRequestOptions :: Maybe (Map.Map String [Text])
    <*> arbitraryReducedMaybe n -- generateVariantsRequestPriceDelta :: Maybe Text
    <*> arbitrary -- generateVariantsRequestProductId :: Text
    <*> arbitraryReducedMaybe n -- generateVariantsRequestSkuPrefix :: Maybe Text
  
instance Arbitrary GewerbesteuerErgebnis where
  arbitrary = sized genGewerbesteuerErgebnis

genGewerbesteuerErgebnis :: Int -> Gen GewerbesteuerErgebnis
genGewerbesteuerErgebnis n =
  GewerbesteuerErgebnis
    <$> arbitrary -- gewerbesteuerErgebnisFreibetrag :: Text
    <*> arbitrary -- gewerbesteuerErgebnisGesamtbelastung :: Text
    <*> arbitrary -- gewerbesteuerErgebnisGewerbeertrag :: Text
    <*> arbitrary -- gewerbesteuerErgebnisHebesatz :: Text
    <*> arbitrary -- gewerbesteuerErgebnisJahr :: Int
    <*> arbitrary -- gewerbesteuerErgebnisKoerperschaftsteuer :: Text
    <*> arbitrary -- gewerbesteuerErgebnisLand :: Text
    <*> arbitrary -- gewerbesteuerErgebnisMessbetrag :: Text
    <*> arbitrary -- gewerbesteuerErgebnisSteuer :: Text
    <*> arbitrary -- gewerbesteuerErgebnisSteuerArt :: Text
  
instance Arbitrary GewinnverwendungsExportResponse where
  arbitrary = sized genGewinnverwendungsExportResponse

genGewinnverwendungsExportResponse :: Int -> Gen GewinnverwendungsExportResponse
genGewinnverwendungsExportResponse n =
  GewinnverwendungsExportResponse
    <$> arbitrary -- gewinnverwendungsExportResponseCsvContent :: Text
    <*> arbitrary -- gewinnverwendungsExportResponseFilename :: Text
  
instance Arbitrary GewinnverwendungsReport where
  arbitrary = sized genGewinnverwendungsReport

genGewinnverwendungsReport :: Int -> Gen GewinnverwendungsReport
genGewinnverwendungsReport n =
  GewinnverwendungsReport
    <$> arbitrary -- gewinnverwendungsReportBilanzgewinn :: Text
    <*> arbitrary -- gewinnverwendungsReportGesetzlicheRuecklageBestand :: Text
    <*> arbitrary -- gewinnverwendungsReportGesetzlicheRuecklageCap :: Text
    <*> arbitrary -- gewinnverwendungsReportGesetzlicheRuecklageNach :: Text
    <*> arbitrary -- gewinnverwendungsReportGesetzlicheRuecklageSoll :: Text
    <*> arbitrary -- gewinnverwendungsReportGezeichnetesKapital :: Text
    <*> arbitrary -- gewinnverwendungsReportJahresueberschuss :: Text
    <*> arbitrary -- gewinnverwendungsReportYear :: Int
    <*> arbitraryReduced n -- gewinnverwendungsReportZeilen :: [GewinnverwendungsZeile]
  
instance Arbitrary GewinnverwendungsZeile where
  arbitrary = sized genGewinnverwendungsZeile

genGewinnverwendungsZeile :: Int -> Gen GewinnverwendungsZeile
genGewinnverwendungsZeile n =
  GewinnverwendungsZeile
    <$> arbitrary -- gewinnverwendungsZeileBetrag :: Text
    <*> arbitrary -- gewinnverwendungsZeileLabel :: Text
  
instance Arbitrary GezReport where
  arbitrary = sized genGezReport

genGezReport :: Int -> Gen GezReport
genGezReport n =
  GezReport
    <$> arbitrary -- gezReportBeitragsfreieKfz :: Integer
    <*> arbitrary -- gezReportBeitragspflichtigeKfz :: Integer
    <*> arbitraryReduced n -- gezReportBetriebsstaetten :: [BetriebsstaettenDetail]
    <*> arbitrary -- gezReportHinweis :: Text
    <*> arbitrary -- gezReportHotelzimmerBeitrag :: Text
    <*> arbitrary -- gezReportJaehrlicherBeitrag :: Text
    <*> arbitrary -- gezReportJahr :: Int
    <*> arbitrary -- gezReportKfzBeitrag :: Text
    <*> arbitrary -- gezReportMonatlicherBeitrag :: Text
    <*> arbitrary -- gezReportVierteljaehrlicherBeitrag :: Text
  
instance Arbitrary GoBDExportResponse where
  arbitrary = sized genGoBDExportResponse

genGoBDExportResponse :: Int -> Gen GoBDExportResponse
genGoBDExportResponse n =
  GoBDExportResponse
    <$> arbitrary -- goBDExportResponseBookingCount :: Int
    <*> arbitrary -- goBDExportResponseCsvContent :: Text
    <*> arbitrary -- goBDExportResponseFilename :: Text
  
instance Arbitrary GoodsReceipt where
  arbitrary = sized genGoodsReceipt

genGoodsReceipt :: Int -> Gen GoodsReceipt
genGoodsReceipt n =
  GoodsReceipt
    <$> arbitrary -- goodsReceiptGrNumber :: Text
    <*> arbitraryReduced n -- goodsReceiptLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- goodsReceiptNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- goodsReceiptPurchaseOrderId :: Maybe Text
    <*> arbitraryReduced n -- goodsReceiptReceiptDate :: Date
    <*> arbitraryReducedMaybe n -- goodsReceiptSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- goodsReceiptSupplierName :: Maybe Text
    <*> arbitrary -- goodsReceiptWarehouseId :: Text
  
instance Arbitrary GroupFigure where
  arbitrary = sized genGroupFigure

genGroupFigure :: Int -> Gen GroupFigure
genGroupFigure n =
  GroupFigure
    <$> arbitraryReducedMaybe n -- groupFigureBilanzsumme :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupFigureExemptionClaimed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- groupFigureMitarbeiter :: Maybe Integer
    <*> arbitraryReducedMaybe n -- groupFigureNettoUmsatz :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupFigureParentName :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupFigureParentSitus :: Maybe Text
    <*> arbitrary -- groupFigureYear :: Int
  
instance Arbitrary GroupFigureCreate where
  arbitrary = sized genGroupFigureCreate

genGroupFigureCreate :: Int -> Gen GroupFigureCreate
genGroupFigureCreate n =
  GroupFigureCreate
    <$> arbitraryReducedMaybe n -- groupFigureCreateBilanzsumme :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupFigureCreateExemptionClaimed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- groupFigureCreateMitarbeiter :: Maybe Integer
    <*> arbitraryReducedMaybe n -- groupFigureCreateNettoUmsatz :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupFigureCreateParentName :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupFigureCreateParentSitus :: Maybe Text
  
instance Arbitrary GroupFigureUpdate where
  arbitrary = sized genGroupFigureUpdate

genGroupFigureUpdate :: Int -> Gen GroupFigureUpdate
genGroupFigureUpdate n =
  GroupFigureUpdate
    <$> arbitraryReducedMaybe n -- groupFigureUpdateBilanzsumme :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupFigureUpdateExemptionClaimed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- groupFigureUpdateMitarbeiter :: Maybe Integer
    <*> arbitraryReducedMaybe n -- groupFigureUpdateNettoUmsatz :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupFigureUpdateParentName :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupFigureUpdateParentSitus :: Maybe Text
  
instance Arbitrary GuVItem where
  arbitrary = sized genGuVItem

genGuVItem :: Int -> Gen GuVItem
genGuVItem n =
  GuVItem
    <$> arbitrary -- guVItemAccount :: Text
    <*> arbitrary -- guVItemAmount :: Text
    <*> arbitrary -- guVItemName :: Text
  
instance Arbitrary GuVReport where
  arbitrary = sized genGuVReport

genGuVReport :: Int -> Gen GuVReport
genGuVReport n =
  GuVReport
    <$> arbitraryReduced n -- guVReportExpenses :: [GuVItem]
    <*> arbitrary -- guVReportGeneratedAt :: Text
    <*> arbitrary -- guVReportNetIncome :: Text
    <*> arbitrary -- guVReportPeriod :: Text
    <*> arbitraryReduced n -- guVReportRevenue :: [GuVItem]
    <*> arbitrary -- guVReportTotalExpenses :: Text
    <*> arbitrary -- guVReportTotalRevenue :: Text
  
instance Arbitrary HebesatzLookup where
  arbitrary = sized genHebesatzLookup

genHebesatzLookup :: Int -> Gen HebesatzLookup
genHebesatzLookup n =
  HebesatzLookup
    <$> arbitrary -- hebesatzLookupBundesland :: Text
    <*> arbitrary -- hebesatzLookupCountryCode :: Text
    <*> arbitrary -- hebesatzLookupGemeindeName :: Text
    <*> arbitrary -- hebesatzLookupGemeindeschluessel :: Text
    <*> arbitrary -- hebesatzLookupHebesatzGewerbesteuer :: Double
    <*> arbitraryReducedMaybe n -- hebesatzLookupHebesatzGrundsteuerB :: Maybe Double
    <*> arbitrary -- hebesatzLookupJahr :: Int
    <*> arbitraryReducedMaybe n -- hebesatzLookupLandkreis :: Maybe Text
    <*> arbitrary -- hebesatzLookupValidFrom :: Text
    <*> arbitraryReducedMaybe n -- hebesatzLookupValidTo :: Maybe Text
  
instance Arbitrary HrTrainingOverview where
  arbitrary = sized genHrTrainingOverview

genHrTrainingOverview :: Int -> Gen HrTrainingOverview
genHrTrainingOverview n =
  HrTrainingOverview
    <$> arbitrary -- hrTrainingOverviewAssignedCount :: Integer
    <*> arbitrary -- hrTrainingOverviewCode :: Text
    <*> arbitrary -- hrTrainingOverviewCompletedCount :: Integer
    <*> arbitrary -- hrTrainingOverviewOverdueCount :: Integer
    <*> arbitrary -- hrTrainingOverviewTitle :: Text
    <*> arbitrary -- hrTrainingOverviewTrainingId :: Text
  
instance Arbitrary ImportJobStatus where
  arbitrary = sized genImportJobStatus

genImportJobStatus :: Int -> Gen ImportJobStatus
genImportJobStatus n =
  ImportJobStatus
    <$> arbitraryReducedMaybe n -- importJobStatusError :: Maybe Text
    <*> arbitrary -- importJobStatusJobId :: Text
    <*> arbitrary -- importJobStatusProcessed :: Integer
    <*> arbitrary -- importJobStatusProgress :: Int
    <*> arbitraryReducedMaybe n -- importJobStatusProvider :: Maybe Text
    <*> arbitrary -- importJobStatusStage :: Text
    <*> arbitrary -- importJobStatusStatus :: Text
    <*> arbitrary -- importJobStatusTotal :: Integer
  
instance Arbitrary ImportStartRequest where
  arbitrary = sized genImportStartRequest

genImportStartRequest :: Int -> Gen ImportStartRequest
genImportStartRequest n =
  ImportStartRequest
    <$> arbitrary -- importStartRequestApiKey :: Text
    <*> arbitrary -- importStartRequestProvider :: Text
    <*> arbitrary -- importStartRequestYears :: [Int]
  
instance Arbitrary ImportStartResponse where
  arbitrary = sized genImportStartResponse

genImportStartResponse :: Int -> Gen ImportStartResponse
genImportStartResponse n =
  ImportStartResponse
    <$> arbitrary -- importStartResponseJobId :: Text
  
instance Arbitrary ImportTestRequest where
  arbitrary = sized genImportTestRequest

genImportTestRequest :: Int -> Gen ImportTestRequest
genImportTestRequest n =
  ImportTestRequest
    <$> arbitrary -- importTestRequestApiKey :: Text
    <*> arbitrary -- importTestRequestProvider :: Text
  
instance Arbitrary ImportTestResponse where
  arbitrary = sized genImportTestResponse

genImportTestResponse :: Int -> Gen ImportTestResponse
genImportTestResponse n =
  ImportTestResponse
    <$> arbitraryReducedMaybe n -- importTestResponseError :: Maybe Text
    <*> arbitrary -- importTestResponseOk :: Bool
  
instance Arbitrary IncomeStatement where
  arbitrary = sized genIncomeStatement

genIncomeStatement :: Int -> Gen IncomeStatement
genIncomeStatement n =
  IncomeStatement
    <$> arbitraryReduced n -- incomeStatementExpenseItems :: [PnLItem]
    <*> arbitrary -- incomeStatementNetIncome :: Text
    <*> arbitraryReduced n -- incomeStatementRevenueItems :: [PnLItem]
    <*> arbitrary -- incomeStatementTotalExpenses :: Text
    <*> arbitrary -- incomeStatementTotalRevenue :: Text
  
instance Arbitrary InstituteCheckItem where
  arbitrary = sized genInstituteCheckItem

genInstituteCheckItem :: Int -> Gen InstituteCheckItem
genInstituteCheckItem n =
  InstituteCheckItem
    <$> arbitrary -- instituteCheckItemExists :: Bool
    <*> arbitrary -- instituteCheckItemName :: Text
    <*> arbitrary -- instituteCheckItemSource :: Text
  
instance Arbitrary InstituteDeadlines where
  arbitrary = sized genInstituteDeadlines

genInstituteDeadlines :: Int -> Gen InstituteDeadlines
genInstituteDeadlines n =
  InstituteDeadlines
    <$> arbitraryReducedMaybe n -- instituteDeadlinesAbschlusspruefungMonths :: Maybe Int
    <*> arbitraryReducedMaybe n -- instituteDeadlinesJahresabschlussBafinMonths :: Maybe Int
    <*> arbitrary -- instituteDeadlinesOffenlegungMonths :: Int
  
instance Arbitrary InstituteProfile where
  arbitrary = sized genInstituteProfile

genInstituteProfile :: Int -> Gen InstituteProfile
genInstituteProfile n =
  InstituteProfile
    <$> arbitraryReducedMaybe n -- instituteProfileInstituteType :: Maybe InstituteType
    <*> arbitraryReducedMaybe n -- instituteProfileKapitalmarktorientiert :: Maybe Bool
  
instance Arbitrary InstituteProfileUpdate where
  arbitrary = sized genInstituteProfileUpdate

genInstituteProfileUpdate :: Int -> Gen InstituteProfileUpdate
genInstituteProfileUpdate n =
  InstituteProfileUpdate
    <$> arbitraryReducedMaybe n -- instituteProfileUpdateInstituteType :: Maybe Text
    <*> arbitraryReducedMaybe n -- instituteProfileUpdateKapitalmarktorientiert :: Maybe Bool
  
instance Arbitrary InstituteStatus where
  arbitrary = sized genInstituteStatus

genInstituteStatus :: Int -> Gen InstituteStatus
genInstituteStatus n =
  InstituteStatus
    <$> arbitraryReduced n -- instituteStatusChecklist :: [InstituteCheckItem]
    <*> arbitraryReduced n -- instituteStatusDeadlines :: InstituteDeadlines
    <*> arbitrary -- instituteStatusInstituteType :: Text
    <*> arbitrary -- instituteStatusKapitalmarktorientiert :: Bool
  
instance Arbitrary InventoryCount where
  arbitrary = sized genInventoryCount

genInventoryCount :: Int -> Gen InventoryCount
genInventoryCount n =
  InventoryCount
    <$> arbitraryReduced n -- inventoryCountCountDate :: Date
    <*> arbitrary -- inventoryCountCountNumber :: Text
    <*> arbitraryReduced n -- inventoryCountLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- inventoryCountNotes :: Maybe Text
    <*> arbitraryReduced n -- inventoryCountStatus :: InventoryCountStatus
    <*> arbitrary -- inventoryCountWarehouseId :: Text
  
instance Arbitrary InventoryCountCreate where
  arbitrary = sized genInventoryCountCreate

genInventoryCountCreate :: Int -> Gen InventoryCountCreate
genInventoryCountCreate n =
  InventoryCountCreate
    <$> arbitraryReduced n -- inventoryCountCreateCountDate :: Date
    <*> arbitrary -- inventoryCountCreateCountNumber :: Text
    <*> arbitraryReduced n -- inventoryCountCreateLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- inventoryCountCreateNotes :: Maybe Text
    <*> arbitraryReduced n -- inventoryCountCreateStatus :: InventoryCountStatus
    <*> arbitrary -- inventoryCountCreateWarehouseId :: Text
  
instance Arbitrary InventoryCountStatusUpdate where
  arbitrary = sized genInventoryCountStatusUpdate

genInventoryCountStatusUpdate :: Int -> Gen InventoryCountStatusUpdate
genInventoryCountStatusUpdate n =
  InventoryCountStatusUpdate
    <$> arbitrary -- inventoryCountStatusUpdateStatus :: Text
  
instance Arbitrary InventoryCountUpdate where
  arbitrary = sized genInventoryCountUpdate

genInventoryCountUpdate :: Int -> Gen InventoryCountUpdate
genInventoryCountUpdate n =
  InventoryCountUpdate
    <$> arbitraryReducedMaybe n -- inventoryCountUpdateCountDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- inventoryCountUpdateCountNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- inventoryCountUpdateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- inventoryCountUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- inventoryCountUpdateStatus :: Maybe InventoryCountStatus
    <*> arbitraryReducedMaybe n -- inventoryCountUpdateWarehouseId :: Maybe Text
  
instance Arbitrary InventoryValuePoint where
  arbitrary = sized genInventoryValuePoint

genInventoryValuePoint :: Int -> Gen InventoryValuePoint
genInventoryValuePoint n =
  InventoryValuePoint
    <$> arbitrary -- inventoryValuePointProductCount :: Integer
    <*> arbitraryReduced n -- inventoryValuePointRecordedAt :: DateTime
    <*> arbitrary -- inventoryValuePointTotalPurchaseValue :: Text
    <*> arbitrary -- inventoryValuePointTotalSalesValue :: Text
  
instance Arbitrary InviteRequest where
  arbitrary = sized genInviteRequest

genInviteRequest :: Int -> Gen InviteRequest
genInviteRequest n =
  InviteRequest
    <$> arbitrary -- inviteRequestEmail :: Text
  
instance Arbitrary Invoice where
  arbitrary = sized genInvoice

genInvoice :: Int -> Gen Invoice
genInvoice n =
  Invoice
    <$> arbitraryReducedMaybe n -- invoiceAttachments :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- invoiceBillingPeriodEnd :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoiceBillingPeriodStart :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoiceCancellationDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoiceCancellationInvoiceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCancellationReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceContractId :: Maybe Text
    <*> arbitraryReduced n -- invoiceCurrency :: CurrencyCode
    <*> arbitraryReducedMaybe n -- invoiceCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceDiscountAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceDiscountDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- invoiceDiscountPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceDocumentType :: Maybe DocumentType
    <*> arbitraryReducedMaybe n -- invoiceDunningLevel :: Maybe Int
    <*> arbitraryReducedMaybe n -- invoiceInputVatAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceInputVatDeductible :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceInputVatPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceIntroductionText :: Maybe Text
    <*> arbitraryReduced n -- invoiceInvoiceType :: InvoiceType
    <*> arbitraryReducedMaybe n -- invoiceIsCancelled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceIsDraft :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceIsEuAcquisition :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceIsEuDelivery :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceIsIntraCommunityAcquisition :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceIsReverseCharge :: Maybe Bool
    <*> arbitraryReduced n -- invoiceIssueDate :: Date
    <*> arbitraryReducedMaybe n -- invoiceLedgerAccount :: Maybe Text
    <*> arbitraryReduced n -- invoiceLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- invoiceMargin25a :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceMargin25aGross :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceMargin25aPurchasePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceOrderNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceOriginalPdfPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoicePaidAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoicePaymentDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoicePaymentStatus :: Maybe PaymentStatus
    <*> arbitraryReducedMaybe n -- invoicePaymentTermsText :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoicePrecedingSalesVoucherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoicePrecedingSalesVoucherType :: Maybe PrecedingSalesVoucherType
    <*> arbitraryReducedMaybe n -- invoiceReceiptConfirmationAvailable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceRelatedInvoiceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceRelationshipType :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceSenderSnapshot :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- invoiceSentAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- invoiceServicePeriodEnd :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoiceServicePeriodStart :: Maybe Date
    <*> arbitraryReduced n -- invoiceStatus :: InvoiceStatus
    <*> arbitrary -- invoiceSubtotal :: Text
    <*> arbitraryReducedMaybe n -- invoiceSupplierId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceTaxExemptionReason :: Maybe Text
    <*> arbitrary -- invoiceTotalAmount :: Text
    <*> arbitrary -- invoiceTotalTax :: Text
    <*> arbitraryReducedMaybe n -- invoiceVatCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- invoiceVatSpecialCase :: Maybe Text
  
instance Arbitrary InvoiceCreate where
  arbitrary = sized genInvoiceCreate

genInvoiceCreate :: Int -> Gen InvoiceCreate
genInvoiceCreate n =
  InvoiceCreate
    <$> arbitraryReducedMaybe n -- invoiceCreateAttachments :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- invoiceCreateBillingPeriodEnd :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoiceCreateBillingPeriodStart :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoiceCreateCancellationDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoiceCreateCancellationInvoiceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateCancellationReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateContractId :: Maybe Text
    <*> arbitraryReduced n -- invoiceCreateCurrency :: CurrencyCode
    <*> arbitraryReducedMaybe n -- invoiceCreateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateDiscountAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateDiscountDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- invoiceCreateDiscountPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateDocumentType :: Maybe DocumentType
    <*> arbitraryReducedMaybe n -- invoiceCreateDunningLevel :: Maybe Int
    <*> arbitraryReducedMaybe n -- invoiceCreateInputVatAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateInputVatDeductible :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceCreateInputVatPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateIntroductionText :: Maybe Text
    <*> arbitraryReduced n -- invoiceCreateInvoiceType :: InvoiceType
    <*> arbitraryReducedMaybe n -- invoiceCreateIsCancelled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceCreateIsDraft :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceCreateIsEuAcquisition :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceCreateIsEuDelivery :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceCreateIsIntraCommunityAcquisition :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceCreateIsReverseCharge :: Maybe Bool
    <*> arbitraryReduced n -- invoiceCreateIssueDate :: Date
    <*> arbitraryReducedMaybe n -- invoiceCreateLedgerAccount :: Maybe Text
    <*> arbitraryReduced n -- invoiceCreateLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- invoiceCreateMargin25a :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceCreateMargin25aGross :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateMargin25aPurchasePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateOrderNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateOriginalPdfPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreatePaidAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreatePaymentDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoiceCreatePaymentStatus :: Maybe PaymentStatus
    <*> arbitraryReducedMaybe n -- invoiceCreatePaymentTermsText :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreatePrecedingSalesVoucherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreatePrecedingSalesVoucherType :: Maybe PrecedingSalesVoucherType
    <*> arbitraryReducedMaybe n -- invoiceCreateReceiptConfirmationAvailable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceCreateRelatedInvoiceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateRelationshipType :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateSenderSnapshot :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- invoiceCreateSentAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- invoiceCreateServicePeriodEnd :: Maybe Date
    <*> arbitraryReducedMaybe n -- invoiceCreateServicePeriodStart :: Maybe Date
    <*> arbitraryReduced n -- invoiceCreateStatus :: InvoiceStatus
    <*> arbitrary -- invoiceCreateSubtotal :: Text
    <*> arbitraryReducedMaybe n -- invoiceCreateSupplierId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceCreateTaxExemptionReason :: Maybe Text
    <*> arbitrary -- invoiceCreateTotalAmount :: Text
    <*> arbitrary -- invoiceCreateTotalTax :: Text
    <*> arbitraryReducedMaybe n -- invoiceCreateVatCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- invoiceCreateVatSpecialCase :: Maybe Text
  
instance Arbitrary InvoiceLineItem where
  arbitrary = sized genInvoiceLineItem

genInvoiceLineItem :: Int -> Gen InvoiceLineItem
genInvoiceLineItem n =
  InvoiceLineItem
    <$> arbitraryReducedMaybe n -- invoiceLineItemArticleNumber :: Maybe Text
    <*> arbitrary -- invoiceLineItemDescription :: Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemDiscountAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemDiscountPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemInputVatDeductible :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceLineItemInputVatRate :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemIsIntraCommunityAcquisition :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceLineItemIsMargin25a :: Maybe Bool
    <*> arbitraryReducedMaybe n -- invoiceLineItemLedgerAccount :: Maybe Text
    <*> arbitrary -- invoiceLineItemLineTotal :: Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemLineTotalGross :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemMargin25aPurchasePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemMeterPointId :: Maybe Text
    <*> arbitrary -- invoiceLineItemPosition :: Integer
    <*> arbitraryReducedMaybe n -- invoiceLineItemPriceComponents :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- invoiceLineItemProductId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemProductSku :: Maybe Text
    <*> arbitrary -- invoiceLineItemQuantity :: Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemSupplierArticleNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemTaxRate :: Maybe Text
    <*> arbitraryReduced n -- invoiceLineItemUnit :: AnyType
    <*> arbitrary -- invoiceLineItemUnitPrice :: Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemUsageDataId :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemVatRateNominal :: Maybe Text
    <*> arbitraryReducedMaybe n -- invoiceLineItemVatSpecialCase :: Maybe Text
  
instance Arbitrary InvoiceMatchRequest where
  arbitrary = sized genInvoiceMatchRequest

genInvoiceMatchRequest :: Int -> Gen InvoiceMatchRequest
genInvoiceMatchRequest n =
  InvoiceMatchRequest
    <$> arbitrary -- invoiceMatchRequestSupplierInvoiceId :: Text
  
instance Arbitrary InvoicePdfUrlResponse where
  arbitrary = sized genInvoicePdfUrlResponse

genInvoicePdfUrlResponse :: Int -> Gen InvoicePdfUrlResponse
genInvoicePdfUrlResponse n =
  InvoicePdfUrlResponse
    <$> arbitrary -- invoicePdfUrlResponseUrl :: Text
  
instance Arbitrary JahresUstErgebnis where
  arbitrary = sized genJahresUstErgebnis

genJahresUstErgebnis :: Int -> Gen JahresUstErgebnis
genJahresUstErgebnis n =
  JahresUstErgebnis
    <$> arbitrary -- jahresUstErgebnisBis :: Text
    <*> arbitrary -- jahresUstErgebnisGespeichertePerioden :: Int
    <*> arbitrary -- jahresUstErgebnisHatIgTransaktionen :: Bool
    <*> arbitrary -- jahresUstErgebnisIstKleinunternehmer :: Bool
    <*> arbitrary -- jahresUstErgebnisJahr :: Int
    <*> arbitrary -- jahresUstErgebnisKz41 :: Text
    <*> arbitrary -- jahresUstErgebnisKz43 :: Text
    <*> arbitrary -- jahresUstErgebnisKz46 :: Text
    <*> arbitrary -- jahresUstErgebnisKz47 :: Text
    <*> arbitrary -- jahresUstErgebnisKz48 :: Text
    <*> arbitrary -- jahresUstErgebnisKz61 :: Text
    <*> arbitrary -- jahresUstErgebnisKz66 :: Text
    <*> arbitrary -- jahresUstErgebnisKz67 :: Text
    <*> arbitrary -- jahresUstErgebnisKz81 :: Text
    <*> arbitrary -- jahresUstErgebnisKz83 :: Text
    <*> arbitrary -- jahresUstErgebnisKz84 :: Text
    <*> arbitrary -- jahresUstErgebnisKz85 :: Text
    <*> arbitrary -- jahresUstErgebnisKz86 :: Text
    <*> arbitrary -- jahresUstErgebnisKz88 :: Text
    <*> arbitrary -- jahresUstErgebnisKz89 :: Text
    <*> arbitrary -- jahresUstErgebnisKz93 :: Text
    <*> arbitrary -- jahresUstErgebnisRestschuld :: Text
    <*> arbitrary -- jahresUstErgebnisSummeVorauszahlungen :: Text
    <*> arbitrary -- jahresUstErgebnisVon :: Text
    <*> arbitrary -- jahresUstErgebnisZahllast :: Text
  
instance Arbitrary Job where
  arbitrary = sized genJob

genJob :: Int -> Gen Job
genJob n =
  Job
    <$> arbitraryReducedMaybe n -- jobAttempts :: Maybe Int
    <*> arbitrary -- jobJobType :: Text
    <*> arbitrary -- jobMaxAttempts :: Int
    <*> arbitraryReducedMaybe n -- jobPayload :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- jobRunAt :: Maybe DateTime
    <*> arbitraryReduced n -- jobStatus :: JobStatus
  
instance Arbitrary JobApplication where
  arbitrary = sized genJobApplication

genJobApplication :: Int -> Gen JobApplication
genJobApplication n =
  JobApplication
    <$> arbitraryReducedMaybe n -- jobApplicationCvFile :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobApplicationCvText :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobApplicationEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobApplicationMatchReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobApplicationMatchScore :: Maybe Int
    <*> arbitraryReducedMaybe n -- jobApplicationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobApplicationPhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobApplicationPostingId :: Maybe Text
    <*> arbitrary -- jobApplicationSource :: Text
    <*> arbitraryReduced n -- jobApplicationStatus :: ApplicationStatus
  
instance Arbitrary JobPosting where
  arbitrary = sized genJobPosting

genJobPosting :: Int -> Gen JobPosting
genJobPosting n =
  JobPosting
    <$> arbitraryReducedMaybe n -- jobPostingCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobPostingDepartment :: Maybe Text
    <*> arbitrary -- jobPostingDescription :: Text
    <*> arbitraryReducedMaybe n -- jobPostingEmploymentType :: Maybe EmploymentType
    <*> arbitraryReducedMaybe n -- jobPostingLocation :: Maybe Text
    <*> arbitrary -- jobPostingRemote :: Bool
    <*> arbitraryReduced n -- jobPostingRequiredSkills :: AnyType
    <*> arbitraryReducedMaybe n -- jobPostingRequirements :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobPostingSalaryMax :: Maybe Int
    <*> arbitraryReducedMaybe n -- jobPostingSalaryMin :: Maybe Int
    <*> arbitraryReduced n -- jobPostingStatus :: JobPostingStatus
    <*> arbitrary -- jobPostingTitle :: Text
  
instance Arbitrary JobPostingCreate where
  arbitrary = sized genJobPostingCreate

genJobPostingCreate :: Int -> Gen JobPostingCreate
genJobPostingCreate n =
  JobPostingCreate
    <$> arbitraryReducedMaybe n -- jobPostingCreateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobPostingCreateDepartment :: Maybe Text
    <*> arbitrary -- jobPostingCreateDescription :: Text
    <*> arbitraryReducedMaybe n -- jobPostingCreateEmploymentType :: Maybe EmploymentType
    <*> arbitraryReducedMaybe n -- jobPostingCreateLocation :: Maybe Text
    <*> arbitrary -- jobPostingCreateRemote :: Bool
    <*> arbitraryReduced n -- jobPostingCreateRequiredSkills :: AnyType
    <*> arbitraryReducedMaybe n -- jobPostingCreateRequirements :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobPostingCreateSalaryMax :: Maybe Int
    <*> arbitraryReducedMaybe n -- jobPostingCreateSalaryMin :: Maybe Int
    <*> arbitraryReduced n -- jobPostingCreateStatus :: JobPostingStatus
    <*> arbitrary -- jobPostingCreateTitle :: Text
  
instance Arbitrary JobPostingFilter where
  arbitrary = sized genJobPostingFilter

genJobPostingFilter :: Int -> Gen JobPostingFilter
genJobPostingFilter n =
  JobPostingFilter
    <$> arbitraryReducedMaybe n -- jobPostingFilterPage :: Maybe Int
    <*> arbitraryReducedMaybe n -- jobPostingFilterPageSize :: Maybe Int
    <*> arbitraryReducedMaybe n -- jobPostingFilterStatus :: Maybe Text
  
instance Arbitrary JobPostingUpdate where
  arbitrary = sized genJobPostingUpdate

genJobPostingUpdate :: Int -> Gen JobPostingUpdate
genJobPostingUpdate n =
  JobPostingUpdate
    <$> arbitraryReducedMaybe n -- jobPostingUpdateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobPostingUpdateDepartment :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobPostingUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobPostingUpdateEmploymentType :: Maybe EmploymentType
    <*> arbitraryReducedMaybe n -- jobPostingUpdateLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobPostingUpdateRemote :: Maybe Bool
    <*> arbitraryReducedMaybe n -- jobPostingUpdateRequiredSkills :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- jobPostingUpdateRequirements :: Maybe Text
    <*> arbitraryReducedMaybe n -- jobPostingUpdateSalaryMax :: Maybe Int
    <*> arbitraryReducedMaybe n -- jobPostingUpdateSalaryMin :: Maybe Int
    <*> arbitraryReducedMaybe n -- jobPostingUpdateStatus :: Maybe JobPostingStatus
    <*> arbitraryReducedMaybe n -- jobPostingUpdateTitle :: Maybe Text
  
instance Arbitrary JobTitleGap where
  arbitrary = sized genJobTitleGap

genJobTitleGap :: Int -> Gen JobTitleGap
genJobTitleGap n =
  JobTitleGap
    <$> arbitrary -- jobTitleGapEmployeeCount :: Int
    <*> arbitrary -- jobTitleGapFemaleMeanHourly :: Text
    <*> arbitrary -- jobTitleGapJobTitle :: Text
    <*> arbitrary -- jobTitleGapMaleMeanHourly :: Text
    <*> arbitrary -- jobTitleGapMeanGapPct :: Double
    <*> arbitrary -- jobTitleGapMedianGapPct :: Double
  
instance Arbitrary KontoItem where
  arbitrary = sized genKontoItem

genKontoItem :: Int -> Gen KontoItem
genKontoItem n =
  KontoItem
    <$> arbitrary -- kontoItemAnfangsbestand :: Text
    <*> arbitrary -- kontoItemHabenUmsatz :: Text
    <*> arbitrary -- kontoItemKonto :: Text
    <*> arbitrary -- kontoItemName :: Text
    <*> arbitrary -- kontoItemSaldo :: Text
    <*> arbitrary -- kontoItemSollUmsatz :: Text
  
instance Arbitrary KontoReport where
  arbitrary = sized genKontoReport

genKontoReport :: Int -> Gen KontoReport
genKontoReport n =
  KontoReport
    <$> arbitrary -- kontoReportGeneratedAt :: Text
    <*> arbitraryReduced n -- kontoReportKonten :: [KontoItem]
    <*> arbitrary -- kontoReportPeriod :: Text
  
instance Arbitrary KonzernBeteiligung where
  arbitrary = sized genKonzernBeteiligung

genKonzernBeteiligung :: Int -> Gen KonzernBeteiligung
genKonzernBeteiligung n =
  KonzernBeteiligung
    <$> arbitrary -- konzernBeteiligungCompanyName :: Text
    <*> arbitrary -- konzernBeteiligungControlBasis :: [Text]
    <*> arbitrary -- konzernBeteiligungControlled :: Bool
    <*> arbitrary -- konzernBeteiligungOwnershipPct :: Text
  
instance Arbitrary KonzernExportResponse where
  arbitrary = sized genKonzernExportResponse

genKonzernExportResponse :: Int -> Gen KonzernExportResponse
genKonzernExportResponse n =
  KonzernExportResponse
    <$> arbitrary -- konzernExportResponseCsvContent :: Text
    <*> arbitrary -- konzernExportResponseFilename :: Text
  
instance Arbitrary KonzernStatus where
  arbitrary = sized genKonzernStatus

genKonzernStatus :: Int -> Gen KonzernStatus
genKonzernStatus n =
  KonzernStatus
    <$> arbitrary -- konzernStatusGroessenbefreit :: Bool
    <*> arbitrary -- konzernStatusKapitalmarktorientiert :: Bool
    <*> arbitrary -- konzernStatusKonzernabschlusspflicht :: Bool
    <*> arbitrary -- konzernStatusMissingGroupFigures :: Bool
    <*> arbitrary -- konzernStatusMutterunternehmen :: Bool
    <*> arbitraryReducedMaybe n -- konzernStatusParentName :: Maybe Text
    <*> arbitraryReducedMaybe n -- konzernStatusParentSitus :: Maybe Text
    <*> arbitraryReduced n -- konzernStatusParticipations :: [KonzernBeteiligung]
    <*> arbitraryReduced n -- konzernStatusThresholds :: KonzernThresholds
    <*> arbitrary -- konzernStatusYear :: Int
    <*> arbitrary -- konzernStatusZwischenholdingBefreit :: Bool
    <*> arbitraryReducedMaybe n -- konzernStatusZwischenholdingHinweis :: Maybe Text
  
instance Arbitrary KonzernThresholds where
  arbitrary = sized genKonzernThresholds

genKonzernThresholds :: Int -> Gen KonzernThresholds
genKonzernThresholds n =
  KonzernThresholds
    <$> arbitrary -- konzernThresholdsBilanzsumme :: Text
    <*> arbitrary -- konzernThresholdsMitarbeiter :: Integer
    <*> arbitrary -- konzernThresholdsNettoUmsatz :: Text
  
instance Arbitrary KostenEintrag where
  arbitrary = sized genKostenEintrag

genKostenEintrag :: Int -> Gen KostenEintrag
genKostenEintrag n =
  KostenEintrag
    <$> arbitrary -- kostenEintragBeschreibung :: Text
    <*> arbitrary -- kostenEintragBetrag :: Text
    <*> arbitrary -- kostenEintragDatum :: Text
    <*> arbitrary -- kostenEintragTyp :: Text
  
instance Arbitrary KostenVorschau where
  arbitrary = sized genKostenVorschau

genKostenVorschau :: Int -> Gen KostenVorschau
genKostenVorschau n =
  KostenVorschau
    <$> arbitraryReduced n -- kostenVorschauEintraege :: [KostenEintrag]
    <*> arbitrary -- kostenVorschauGesamt :: Text
  
instance Arbitrary KstErgebnis where
  arbitrary = sized genKstErgebnis

genKstErgebnis :: Int -> Gen KstErgebnis
genKstErgebnis n =
  KstErgebnis
    <$> arbitrary -- kstErgebnisGesamt :: Text
    <*> arbitrary -- kstErgebnisGesamtbelastung :: Text
    <*> arbitrary -- kstErgebnisGewerbesteuer :: Text
    <*> arbitrary -- kstErgebnisGewinn :: Text
    <*> arbitrary -- kstErgebnisIstKapitalgesellschaft :: Bool
    <*> arbitrary -- kstErgebnisJahr :: Int
    <*> arbitrary -- kstErgebnisKoerperschaftsteuer :: Text
    <*> arbitrary -- kstErgebnisSolidaritaetszuschlag :: Text
  
instance Arbitrary KycRecord where
  arbitrary = sized genKycRecord

genKycRecord :: Int -> Gen KycRecord
genKycRecord n =
  KycRecord
    <$> arbitraryReducedMaybe n -- kycRecordCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- kycRecordCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- kycRecordKycDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- kycRecordNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- kycRecordRetentionUntil :: Maybe Date
    <*> arbitraryReducedMaybe n -- kycRecordRiskAssessment :: Maybe Text
  
instance Arbitrary KycRecordCreate where
  arbitrary = sized genKycRecordCreate

genKycRecordCreate :: Int -> Gen KycRecordCreate
genKycRecordCreate n =
  KycRecordCreate
    <$> arbitraryReducedMaybe n -- kycRecordCreateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- kycRecordCreateCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- kycRecordCreateKycDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- kycRecordCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- kycRecordCreateRetentionUntil :: Maybe Date
    <*> arbitraryReducedMaybe n -- kycRecordCreateRiskAssessment :: Maybe Text
  
instance Arbitrary KycRecordUpdate where
  arbitrary = sized genKycRecordUpdate

genKycRecordUpdate :: Int -> Gen KycRecordUpdate
genKycRecordUpdate n =
  KycRecordUpdate
    <$> arbitraryReducedMaybe n -- kycRecordUpdateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- kycRecordUpdateCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- kycRecordUpdateKycDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- kycRecordUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- kycRecordUpdateRetentionUntil :: Maybe Date
    <*> arbitraryReducedMaybe n -- kycRecordUpdateRiskAssessment :: Maybe Text
  
instance Arbitrary LaborCostRow where
  arbitrary = sized genLaborCostRow

genLaborCostRow :: Int -> Gen LaborCostRow
genLaborCostRow n =
  LaborCostRow
    <$> arbitrary -- laborCostRowCost :: Text
    <*> arbitraryReducedMaybe n -- laborCostRowEmployeeId :: Maybe Text
    <*> arbitrary -- laborCostRowGroupKey :: Text
    <*> arbitrary -- laborCostRowHours :: Text
    <*> arbitraryReducedMaybe n -- laborCostRowName :: Maybe Text
  
instance Arbitrary Lead where
  arbitrary = sized genLead

genLead :: Int -> Gen Lead
genLead n =
  Lead
    <$> arbitraryReducedMaybe n -- leadCompany :: Maybe Text
    <*> arbitraryReducedMaybe n -- leadConvertedAt :: Maybe DateTime
    <*> arbitraryReduced n -- leadCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- leadEmail :: Maybe Text
    <*> arbitraryReduced n -- leadFirstContactAt :: DateTime
    <*> arbitrary -- leadName :: Text
    <*> arbitraryReducedMaybe n -- leadNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- leadPhone :: Maybe Text
    <*> arbitrary -- leadScore :: Int
    <*> arbitrary -- leadSource :: Text
    <*> arbitraryReduced n -- leadStatus :: LeadStatus
    <*> arbitraryReduced n -- leadTags :: AnyType
    <*> arbitrary -- leadTenantId :: Text
    <*> arbitraryReducedMaybe n -- leadUpdatedAt :: Maybe DateTime
  
instance Arbitrary LeadUpdate where
  arbitrary = sized genLeadUpdate

genLeadUpdate :: Int -> Gen LeadUpdate
genLeadUpdate n =
  LeadUpdate
    <$> arbitraryReducedMaybe n -- leadUpdateCompany :: Maybe Text
    <*> arbitraryReducedMaybe n -- leadUpdateConvertedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- leadUpdateCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- leadUpdateEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- leadUpdateFirstContactAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- leadUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- leadUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- leadUpdatePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- leadUpdateScore :: Maybe Int
    <*> arbitraryReducedMaybe n -- leadUpdateSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- leadUpdateStatus :: Maybe LeadStatus
    <*> arbitraryReducedMaybe n -- leadUpdateTags :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- leadUpdateTenantId :: Maybe Text
    <*> arbitraryReducedMaybe n -- leadUpdateUpdatedAt :: Maybe DateTime
  
instance Arbitrary LegalDocument where
  arbitrary = sized genLegalDocument

genLegalDocument :: Int -> Gen LegalDocument
genLegalDocument n =
  LegalDocument
    <$> arbitrary -- legalDocumentContent :: Text
    <*> arbitraryReduced n -- legalDocumentDocType :: LegalDocType
    <*> arbitraryReduced n -- legalDocumentLang :: LanguageCode
    <*> arbitrary -- legalDocumentTitle :: Text
  
instance Arbitrary LegalDocumentReset where
  arbitrary = sized genLegalDocumentReset

genLegalDocumentReset :: Int -> Gen LegalDocumentReset
genLegalDocumentReset n =
  LegalDocumentReset
    <$> arbitraryReducedMaybe n -- legalDocumentResetDocType :: Maybe Text
    <*> arbitraryReducedMaybe n -- legalDocumentResetLang :: Maybe Text
  
instance Arbitrary LegalDocumentUpsert where
  arbitrary = sized genLegalDocumentUpsert

genLegalDocumentUpsert :: Int -> Gen LegalDocumentUpsert
genLegalDocumentUpsert n =
  LegalDocumentUpsert
    <$> arbitrary -- legalDocumentUpsertContent :: Text
    <*> arbitrary -- legalDocumentUpsertDocType :: Text
    <*> arbitrary -- legalDocumentUpsertLang :: Text
    <*> arbitrary -- legalDocumentUpsertTitle :: Text
  
instance Arbitrary LiquidityPosition where
  arbitrary = sized genLiquidityPosition

genLiquidityPosition :: Int -> Gen LiquidityPosition
genLiquidityPosition n =
  LiquidityPosition
    <$> arbitrary -- liquidityPositionAccountsPayable :: Double
    <*> arbitrary -- liquidityPositionAccountsReceivable :: Double
    <*> arbitrary -- liquidityPositionCashAndEquivalents :: Double
    <*> arbitrary -- liquidityPositionCurrentRatio :: Double
    <*> arbitrary -- liquidityPositionQuickRatio :: Double
    <*> arbitrary -- liquidityPositionWorkingCapital :: Double
  
instance Arbitrary LoginRequest where
  arbitrary = sized genLoginRequest

genLoginRequest :: Int -> Gen LoginRequest
genLoginRequest n =
  LoginRequest
    <$> arbitrary -- loginRequestEmail :: Text
    <*> arbitrary -- loginRequestPassword :: Text
    <*> arbitraryReducedMaybe n -- loginRequestTotpCode :: Maybe Text
  
instance Arbitrary MagicLinkRequest where
  arbitrary = sized genMagicLinkRequest

genMagicLinkRequest :: Int -> Gen MagicLinkRequest
genMagicLinkRequest n =
  MagicLinkRequest
    <$> arbitrary -- magicLinkRequestEmail :: Text
  
instance Arbitrary MagicLinkVerifyRequest where
  arbitrary = sized genMagicLinkVerifyRequest

genMagicLinkVerifyRequest :: Int -> Gen MagicLinkVerifyRequest
genMagicLinkVerifyRequest n =
  MagicLinkVerifyRequest
    <$> arbitrary -- magicLinkVerifyRequestToken :: Text
  
instance Arbitrary MarketplaceConnection where
  arbitrary = sized genMarketplaceConnection

genMarketplaceConnection :: Int -> Gen MarketplaceConnection
genMarketplaceConnection n =
  MarketplaceConnection
    <$> arbitraryReduced n -- marketplaceConnectionConfig :: AnyType
    <*> arbitrary -- marketplaceConnectionConnectionId :: Text
    <*> arbitraryReduced n -- marketplaceConnectionConnectorType :: ConnectorType
    <*> arbitraryReduced n -- marketplaceConnectionCreatedAt :: DateTime
    <*> arbitrary -- marketplaceConnectionIsActive :: Bool
    <*> arbitrary -- marketplaceConnectionLabel :: Text
    <*> arbitraryReducedMaybe n -- marketplaceConnectionLastSyncAt :: Maybe DateTime
    <*> arbitrary -- marketplaceConnectionPlatform :: Text
    <*> arbitraryReducedMaybe n -- marketplaceConnectionPlatformUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- marketplaceConnectionScopes :: Maybe Text
    <*> arbitraryReducedMaybe n -- marketplaceConnectionShopDomain :: Maybe Text
    <*> arbitraryReducedMaybe n -- marketplaceConnectionShopName :: Maybe Text
    <*> arbitraryReducedMaybe n -- marketplaceConnectionSyncStatus :: Maybe Text
    <*> arbitrary -- marketplaceConnectionTenantId :: Text
    <*> arbitraryReducedMaybe n -- marketplaceConnectionUpdatedAt :: Maybe DateTime
  
instance Arbitrary MarketplaceSyncLog where
  arbitrary = sized genMarketplaceSyncLog

genMarketplaceSyncLog :: Int -> Gen MarketplaceSyncLog
genMarketplaceSyncLog n =
  MarketplaceSyncLog
    <$> arbitraryReducedMaybe n -- marketplaceSyncLogCompletedAt :: Maybe DateTime
    <*> arbitrary -- marketplaceSyncLogConnectionId :: Text
    <*> arbitraryReducedMaybe n -- marketplaceSyncLogErrorMessage :: Maybe Text
    <*> arbitrary -- marketplaceSyncLogItemsFailed :: Int
    <*> arbitrary -- marketplaceSyncLogItemsSynced :: Int
    <*> arbitrary -- marketplaceSyncLogPlatform :: Text
    <*> arbitraryReduced n -- marketplaceSyncLogStartedAt :: DateTime
    <*> arbitraryReduced n -- marketplaceSyncLogStatus :: SyncLogStatus
    <*> arbitraryReduced n -- marketplaceSyncLogSyncType :: SyncType
  
instance Arbitrary MarketplaceWebhookEvent where
  arbitrary = sized genMarketplaceWebhookEvent

genMarketplaceWebhookEvent :: Int -> Gen MarketplaceWebhookEvent
genMarketplaceWebhookEvent n =
  MarketplaceWebhookEvent
    <$> arbitrary -- marketplaceWebhookEventConnectionId :: Text
    <*> arbitraryReducedMaybe n -- marketplaceWebhookEventEventBody :: Maybe AnyType
    <*> arbitrary -- marketplaceWebhookEventEventType :: Text
    <*> arbitraryReducedMaybe n -- marketplaceWebhookEventHeaders :: Maybe AnyType
    <*> arbitrary -- marketplaceWebhookEventPlatform :: Text
    <*> arbitraryReducedMaybe n -- marketplaceWebhookEventProcessed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- marketplaceWebhookEventProcessingError :: Maybe Text
  
instance Arbitrary MeteredUsage where
  arbitrary = sized genMeteredUsage

genMeteredUsage :: Int -> Gen MeteredUsage
genMeteredUsage n =
  MeteredUsage
    <$> arbitrary -- meteredUsageLimit :: Integer
    <*> arbitrary -- meteredUsageMeter :: Text
    <*> arbitrary -- meteredUsageUsed :: Integer
  
instance Arbitrary MethodSuitability where
  arbitrary = sized genMethodSuitability

genMethodSuitability :: Int -> Gen MethodSuitability
genMethodSuitability n =
  MethodSuitability
    <$> arbitrary -- methodSuitabilityCarrier :: Text
    <*> arbitraryReducedMaybe n -- methodSuitabilityRate :: Maybe ShippingRate
    <*> arbitrary -- methodSuitabilityReasons :: [Text]
    <*> arbitrary -- methodSuitabilityService :: Text
    <*> arbitrary -- methodSuitabilitySuitable :: Bool
  
instance Arbitrary MirrorTriggerResponse where
  arbitrary = sized genMirrorTriggerResponse

genMirrorTriggerResponse :: Int -> Gen MirrorTriggerResponse
genMirrorTriggerResponse n =
  MirrorTriggerResponse
    <$> arbitrary -- mirrorTriggerResponseJobId :: Text
  
instance Arbitrary Model where
  arbitrary = sized genModel

genModel :: Int -> Gen Model
genModel n =
  Model
    <$> arbitrary -- modelBackupCodes :: [Text]
    <*> arbitraryReduced n -- modelCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- modelDeletedAt :: Maybe DateTime
    <*> arbitrary -- modelEmail :: Text
    <*> arbitrary -- modelEmailVerified :: Bool
    <*> arbitrary -- modelId :: Text
    <*> arbitrary -- modelIsActive :: Bool
    <*> arbitrary -- modelIsTotpEnabled :: Bool
    <*> arbitraryReducedMaybe n -- modelLastLogin :: Maybe DateTime
    <*> arbitrary -- modelName :: Text
    <*> arbitraryReducedMaybe n -- modelOauthId :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelOauthProvider :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelPasswordChangedAt :: Maybe DateTime
    <*> arbitrary -- modelPasswordHash :: Text
    <*> arbitraryReducedMaybe n -- modelPicture :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelPrivacyAcceptedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- modelTotpSecret :: Maybe Text
    <*> arbitraryReduced n -- modelUpdatedAt :: DateTime
  
instance Arbitrary MyTrainingItem where
  arbitrary = sized genMyTrainingItem

genMyTrainingItem :: Int -> Gen MyTrainingItem
genMyTrainingItem n =
  MyTrainingItem
    <$> arbitrary -- myTrainingItemAssignmentId :: Text
    <*> arbitraryReducedMaybe n -- myTrainingItemCertificateId :: Maybe Text
    <*> arbitrary -- myTrainingItemCode :: Text
    <*> arbitraryReducedMaybe n -- myTrainingItemDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- myTrainingItemDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- myTrainingItemLastScore :: Maybe Int
    <*> arbitrary -- myTrainingItemPassScore :: Int
    <*> arbitraryReducedMaybe n -- myTrainingItemPassed :: Maybe Bool
    <*> arbitraryReduced n -- myTrainingItemStatus :: AssignmentStatus
    <*> arbitrary -- myTrainingItemTitle :: Text
    <*> arbitrary -- myTrainingItemTrainingId :: Text
    <*> arbitraryReducedMaybe n -- myTrainingItemValidUntil :: Maybe DateTime
  
instance Arbitrary NewVersionRequest where
  arbitrary = sized genNewVersionRequest

genNewVersionRequest :: Int -> Gen NewVersionRequest
genNewVersionRequest n =
  NewVersionRequest
    <$> arbitrary -- newVersionRequestFileName :: Text
    <*> arbitraryReducedMaybe n -- newVersionRequestFileSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- newVersionRequestMimeType :: Maybe Text
    <*> arbitraryReducedMaybe n -- newVersionRequestOriginalName :: Maybe Text
    <*> arbitraryReducedMaybe n -- newVersionRequestSha256Hash :: Maybe Text
  
instance Arbitrary NotificationDto where
  arbitrary = sized genNotificationDto

genNotificationDto :: Int -> Gen NotificationDto
genNotificationDto n =
  NotificationDto
    <$> arbitraryReduced n -- notificationDtoCreatedAt :: DateTime
    <*> arbitrary -- notificationDtoId :: Text
    <*> arbitrary -- notificationDtoIsRead :: Bool
    <*> arbitraryReducedMaybe n -- notificationDtoMessage :: Maybe Text
    <*> arbitrary -- notificationDtoSentViaEmail :: Bool
    <*> arbitrary -- notificationDtoTenantId :: Text
    <*> arbitrary -- notificationDtoTitle :: Text
    <*> arbitrary -- notificationDtoUserId :: Text
  
instance Arbitrary OAuthAuthorizeRequest where
  arbitrary = sized genOAuthAuthorizeRequest

genOAuthAuthorizeRequest :: Int -> Gen OAuthAuthorizeRequest
genOAuthAuthorizeRequest n =
  OAuthAuthorizeRequest
    <$> arbitraryReducedMaybe n -- oAuthAuthorizeRequestConfig :: Maybe AnyType
    <*> arbitrary -- oAuthAuthorizeRequestPlatform :: Text
    <*> arbitrary -- oAuthAuthorizeRequestRedirectUri :: Text
  
instance Arbitrary OAuthAuthorizeResponse where
  arbitrary = sized genOAuthAuthorizeResponse

genOAuthAuthorizeResponse :: Int -> Gen OAuthAuthorizeResponse
genOAuthAuthorizeResponse n =
  OAuthAuthorizeResponse
    <$> arbitrary -- oAuthAuthorizeResponseAuthorizationUrl :: Text
    <*> arbitrary -- oAuthAuthorizeResponseState :: Text
  
instance Arbitrary OAuthCallbackRequest where
  arbitrary = sized genOAuthCallbackRequest

genOAuthCallbackRequest :: Int -> Gen OAuthCallbackRequest
genOAuthCallbackRequest n =
  OAuthCallbackRequest
    <$> arbitrary -- oAuthCallbackRequestCode :: Text
    <*> arbitraryReducedMaybe n -- oAuthCallbackRequestConfig :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- oAuthCallbackRequestConnectionId :: Maybe Text
    <*> arbitrary -- oAuthCallbackRequestPlatform :: Text
    <*> arbitraryReducedMaybe n -- oAuthCallbackRequestShopDomain :: Maybe Text
    <*> arbitrary -- oAuthCallbackRequestState :: Text
  
instance Arbitrary OcrTextRequest where
  arbitrary = sized genOcrTextRequest

genOcrTextRequest :: Int -> Gen OcrTextRequest
genOcrTextRequest n =
  OcrTextRequest
    <$> arbitraryReducedMaybe n -- ocrTextRequestOcrText :: Maybe Text
  
instance Arbitrary OffenlegungItem where
  arbitrary = sized genOffenlegungItem

genOffenlegungItem :: Int -> Gen OffenlegungItem
genOffenlegungItem n =
  OffenlegungItem
    <$> arbitrary -- offenlegungItemExists :: Bool
    <*> arbitrary -- offenlegungItemName :: Text
    <*> arbitrary -- offenlegungItemSource :: Text
  
instance Arbitrary OffenlegungReport where
  arbitrary = sized genOffenlegungReport

genOffenlegungReport :: Int -> Gen OffenlegungReport
genOffenlegungReport n =
  OffenlegungReport
    <$> arbitraryReduced n -- offenlegungReportDeadline :: Date
    <*> arbitrary -- offenlegungReportDeadlineMonths :: Int
    <*> arbitraryReduced n -- offenlegungReportItems :: [OffenlegungItem]
    <*> arbitrary -- offenlegungReportKapitalmarktorientiert :: Bool
    <*> arbitrary -- offenlegungReportNote :: Text
    <*> arbitrary -- offenlegungReportYear :: Int
  
instance Arbitrary OpenItem where
  arbitrary = sized genOpenItem

genOpenItem :: Int -> Gen OpenItem
genOpenItem n =
  OpenItem
    <$> arbitrary -- openItemAmountDue :: Text
    <*> arbitrary -- openItemAmountPaid :: Text
    <*> arbitraryReducedMaybe n -- openItemCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- openItemDaysOverdue :: Maybe Integer
    <*> arbitraryReducedMaybe n -- openItemDueDate :: Maybe Text
    <*> arbitrary -- openItemInvoiceId :: Text
    <*> arbitrary -- openItemInvoiceNumber :: Text
    <*> arbitrary -- openItemIssueDate :: Text
    <*> arbitrary -- openItemOpenAmount :: Text
    <*> arbitraryReduced n -- openItemReminderLevel :: ReminderLevel
  
instance Arbitrary Order where
  arbitrary = sized genOrder

genOrder :: Int -> Gen Order
genOrder n =
  Order
    <$> arbitraryReducedMaybe n -- orderAuditLog :: Maybe AnyType
    <*> arbitrary -- orderCurrency :: Text
    <*> arbitrary -- orderCustomerId :: Text
    <*> arbitraryReducedMaybe n -- orderExternalReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderInvoiceAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderLanguage :: Maybe LanguageCode
    <*> arbitraryReduced n -- orderOrderStatus :: OrderStatus
    <*> arbitraryReduced n -- orderPaymentMethod :: PaymentMethod
    <*> arbitraryReducedMaybe n -- orderShippingAddress :: Maybe AnyType
    <*> arbitrary -- orderShippingCost :: Text
    <*> arbitrary -- orderShippingMethod :: Text
    <*> arbitrary -- orderShippingWeight :: Text
    <*> arbitrary -- orderTags :: [Text]
    <*> arbitrary -- orderTotalCost :: Text
  
instance Arbitrary OrderConfirmation where
  arbitrary = sized genOrderConfirmation

genOrderConfirmation :: Int -> Gen OrderConfirmation
genOrderConfirmation n =
  OrderConfirmation
    <$> arbitraryReducedMaybe n -- orderConfirmationAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderConfirmationConfirmationNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationContactName :: Maybe Text
    <*> arbitrary -- orderConfirmationCurrency :: Text
    <*> arbitraryReducedMaybe n -- orderConfirmationFiles :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderConfirmationIntroduction :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderConfirmationPrecedingSalesVoucherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationPrecedingSalesVoucherType :: Maybe PrecedingSalesVoucherType
    <*> arbitraryReducedMaybe n -- orderConfirmationRemark :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationSubtotal :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationTaxCondition :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationTotalAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationTotalTax :: Maybe Text
    <*> arbitraryReduced n -- orderConfirmationVoucherDate :: Date
    <*> arbitraryReduced n -- orderConfirmationVoucherStatus :: VoucherStatus
  
instance Arbitrary OrderConfirmationCreate where
  arbitrary = sized genOrderConfirmationCreate

genOrderConfirmationCreate :: Int -> Gen OrderConfirmationCreate
genOrderConfirmationCreate n =
  OrderConfirmationCreate
    <$> arbitraryReducedMaybe n -- orderConfirmationCreateAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderConfirmationCreateConfirmationNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationCreateContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationCreateContactName :: Maybe Text
    <*> arbitrary -- orderConfirmationCreateCurrency :: Text
    <*> arbitraryReducedMaybe n -- orderConfirmationCreateFiles :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderConfirmationCreateIntroduction :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationCreateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderConfirmationCreatePrecedingSalesVoucherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationCreatePrecedingSalesVoucherType :: Maybe PrecedingSalesVoucherType
    <*> arbitraryReducedMaybe n -- orderConfirmationCreateRemark :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationCreateTaxCondition :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderConfirmationCreateTitle :: Maybe Text
    <*> arbitraryReduced n -- orderConfirmationCreateVoucherDate :: Date
    <*> arbitraryReduced n -- orderConfirmationCreateVoucherStatus :: VoucherStatus
  
instance Arbitrary OrderCreate where
  arbitrary = sized genOrderCreate

genOrderCreate :: Int -> Gen OrderCreate
genOrderCreate n =
  OrderCreate
    <$> arbitraryReducedMaybe n -- orderCreateAuditLog :: Maybe AnyType
    <*> arbitrary -- orderCreateCurrency :: Text
    <*> arbitrary -- orderCreateCustomerId :: Text
    <*> arbitraryReducedMaybe n -- orderCreateExternalReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderCreateInvoiceAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderCreateItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderCreateLanguage :: Maybe LanguageCode
    <*> arbitraryReduced n -- orderCreateOrderStatus :: OrderStatus
    <*> arbitraryReduced n -- orderCreatePaymentMethod :: PaymentMethod
    <*> arbitraryReducedMaybe n -- orderCreateShippingAddress :: Maybe AnyType
    <*> arbitrary -- orderCreateShippingCost :: Text
    <*> arbitrary -- orderCreateShippingMethod :: Text
    <*> arbitrary -- orderCreateShippingWeight :: Text
    <*> arbitrary -- orderCreateTags :: [Text]
    <*> arbitrary -- orderCreateTotalCost :: Text
  
instance Arbitrary OrderStateUpdate where
  arbitrary = sized genOrderStateUpdate

genOrderStateUpdate :: Int -> Gen OrderStateUpdate
genOrderStateUpdate n =
  OrderStateUpdate
    <$> arbitraryReducedMaybe n -- orderStateUpdateSendStateToShop :: Maybe Bool
    <*> arbitrary -- orderStateUpdateState :: Text
  
instance Arbitrary OrderTagsRequest where
  arbitrary = sized genOrderTagsRequest

genOrderTagsRequest :: Int -> Gen OrderTagsRequest
genOrderTagsRequest n =
  OrderTagsRequest
    <$> arbitrary -- orderTagsRequestTags :: [Text]
  
instance Arbitrary OrderUpdate where
  arbitrary = sized genOrderUpdate

genOrderUpdate :: Int -> Gen OrderUpdate
genOrderUpdate n =
  OrderUpdate
    <$> arbitraryReducedMaybe n -- orderUpdateAuditLog :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderUpdateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderUpdateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderUpdateExternalReference :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderUpdateInvoiceAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderUpdateItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderUpdateLanguage :: Maybe LanguageCode
    <*> arbitraryReducedMaybe n -- orderUpdateOrderStatus :: Maybe OrderStatus
    <*> arbitraryReducedMaybe n -- orderUpdatePaymentMethod :: Maybe PaymentMethod
    <*> arbitraryReducedMaybe n -- orderUpdateShippingAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- orderUpdateShippingCost :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderUpdateShippingMethod :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderUpdateShippingWeight :: Maybe Text
    <*> arbitraryReducedMaybe n -- orderUpdateTags :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- orderUpdateTotalCost :: Maybe Text
  
instance Arbitrary OssDependency where
  arbitrary = sized genOssDependency

genOssDependency :: Int -> Gen OssDependency
genOssDependency n =
  OssDependency
    <$> arbitrary -- ossDependencyDependencyType :: Text
    <*> arbitraryReducedMaybe n -- ossDependencyLicense :: Maybe Text
    <*> arbitrary -- ossDependencyName :: Text
    <*> arbitrary -- ossDependencyVersion :: Text
  
instance Arbitrary OssReport where
  arbitrary = sized genOssReport

genOssReport :: Int -> Gen OssReport
genOssReport n =
  OssReport
    <$> arbitraryReduced n -- ossReportDependencies :: [OssDependency]
    <*> arbitrary -- ossReportTotalCount :: Int
  
instance Arbitrary Package where
  arbitrary = sized genPackage

genPackage :: Int -> Gen Package
genPackage n =
  Package
    <$> arbitraryReducedMaybe n -- packageDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- packageHeightCm :: Maybe Double
    <*> arbitraryReducedMaybe n -- packageLengthCm :: Maybe Double
    <*> arbitraryReducedMaybe n -- packageReference :: Maybe Text
    <*> arbitrary -- packageWeightKg :: Double
    <*> arbitraryReducedMaybe n -- packageWidthCm :: Maybe Double
  
instance Arbitrary PackingCompleteRequest where
  arbitrary = sized genPackingCompleteRequest

genPackingCompleteRequest :: Int -> Gen PackingCompleteRequest
genPackingCompleteRequest n =
  PackingCompleteRequest
    <$> arbitraryReducedMaybe n -- packingCompleteRequestNotes :: Maybe Text
    <*> arbitrary -- packingCompleteRequestOrderNumber :: Text
    <*> arbitraryReducedMaybe n -- packingCompleteRequestShipmentId :: Maybe Text
    <*> arbitraryReducedMaybe n -- packingCompleteRequestVideoUrl :: Maybe Text
  
instance Arbitrary PackingCompleteResponse where
  arbitrary = sized genPackingCompleteResponse

genPackingCompleteResponse :: Int -> Gen PackingCompleteResponse
genPackingCompleteResponse n =
  PackingCompleteResponse
    <$> arbitrary -- packingCompleteResponseMessage :: Text
    <*> arbitrary -- packingCompleteResponseNewState :: Text
    <*> arbitrary -- packingCompleteResponseOrderNumber :: Text
    <*> arbitrary -- packingCompleteResponseSuccess :: Bool
  
instance Arbitrary PackingQueue where
  arbitrary = sized genPackingQueue

genPackingQueue :: Int -> Gen PackingQueue
genPackingQueue n =
  PackingQueue
    <$> arbitraryReduced n -- packingQueueItems :: [PackingQueueItem]
    <*> arbitrary -- packingQueuePage :: Int
    <*> arbitrary -- packingQueuePageSize :: Int
    <*> arbitrary -- packingQueueTotalCount :: Integer
  
instance Arbitrary PackingQueueItem where
  arbitrary = sized genPackingQueueItem

genPackingQueueItem :: Int -> Gen PackingQueueItem
genPackingQueueItem n =
  PackingQueueItem
    <$> arbitrary -- packingQueueItemCreatedAt :: Text
    <*> arbitrary -- packingQueueItemCustomerId :: Text
    <*> arbitrary -- packingQueueItemDeliveryNotePrinted :: Bool
    <*> arbitraryReduced n -- packingQueueItemItems :: AnyType
    <*> arbitrary -- packingQueueItemItemsCount :: Int
    <*> arbitrary -- packingQueueItemLabelPrinted :: Bool
    <*> arbitrary -- packingQueueItemOrderNumber :: Text
    <*> arbitrary -- packingQueueItemOrderStatus :: Text
    <*> arbitraryReducedMaybe n -- packingQueueItemShipmentId :: Maybe Text
    <*> arbitraryReducedMaybe n -- packingQueueItemShippingAddress :: Maybe AnyType
    <*> arbitrary -- packingQueueItemShippingMethod :: Text
    <*> arbitraryReducedMaybe n -- packingQueueItemTrackingNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- packingQueueItemVideoRecording :: Maybe Text
  
instance Arbitrary PackingVideoResponse where
  arbitrary = sized genPackingVideoResponse

genPackingVideoResponse :: Int -> Gen PackingVideoResponse
genPackingVideoResponse n =
  PackingVideoResponse
    <$> arbitrary -- packingVideoResponseMessage :: Text
    <*> arbitraryReducedMaybe n -- packingVideoResponseRecordingUrl :: Maybe Text
    <*> arbitrary -- packingVideoResponseSuccess :: Bool
  
instance Arbitrary PartialFeatureSettings where
  arbitrary = sized genPartialFeatureSettings

genPartialFeatureSettings :: Int -> Gen PartialFeatureSettings
genPartialFeatureSettings n =
  PartialFeatureSettings
    <$> arbitraryReducedMaybe n -- partialFeatureSettingsOnlineshop :: Maybe Bool
    <*> arbitraryReducedMaybe n -- partialFeatureSettingsReportBilanz :: Maybe Bool
    <*> arbitraryReducedMaybe n -- partialFeatureSettingsReportBwa :: Maybe Bool
    <*> arbitraryReducedMaybe n -- partialFeatureSettingsReportEuer :: Maybe Bool
    <*> arbitraryReducedMaybe n -- partialFeatureSettingsReportGewerbesteuer :: Maybe Bool
    <*> arbitraryReducedMaybe n -- partialFeatureSettingsReportGuv :: Maybe Bool
    <*> arbitraryReducedMaybe n -- partialFeatureSettingsReportKst :: Maybe Bool
    <*> arbitraryReducedMaybe n -- partialFeatureSettingsReportUstva :: Maybe Bool
  
instance Arbitrary Participation where
  arbitrary = sized genParticipation

genParticipation :: Int -> Gen Participation
genParticipation n =
  Participation
    <$> arbitraryReducedMaybe n -- participationAcquiredAt :: Maybe Date
    <*> arbitraryReducedMaybe n -- participationBoardAppointment :: Maybe Bool
    <*> arbitraryReducedMaybe n -- participationCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- participationControlAgreement :: Maybe Bool
    <*> arbitraryReducedMaybe n -- participationLegalForm :: Maybe Text
    <*> arbitraryReducedMaybe n -- participationOwnershipPct :: Maybe Text
    <*> arbitraryReducedMaybe n -- participationPurposeVehicle :: Maybe Bool
    <*> arbitraryReducedMaybe n -- participationVotingMajority :: Maybe Bool
  
instance Arbitrary ParticipationCreate where
  arbitrary = sized genParticipationCreate

genParticipationCreate :: Int -> Gen ParticipationCreate
genParticipationCreate n =
  ParticipationCreate
    <$> arbitraryReducedMaybe n -- participationCreateAcquiredAt :: Maybe Date
    <*> arbitraryReducedMaybe n -- participationCreateBoardAppointment :: Maybe Bool
    <*> arbitraryReducedMaybe n -- participationCreateCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- participationCreateControlAgreement :: Maybe Bool
    <*> arbitraryReducedMaybe n -- participationCreateLegalForm :: Maybe Text
    <*> arbitraryReducedMaybe n -- participationCreateOwnershipPct :: Maybe Text
    <*> arbitraryReducedMaybe n -- participationCreatePurposeVehicle :: Maybe Bool
    <*> arbitraryReducedMaybe n -- participationCreateVotingMajority :: Maybe Bool
  
instance Arbitrary ParticipationUpdate where
  arbitrary = sized genParticipationUpdate

genParticipationUpdate :: Int -> Gen ParticipationUpdate
genParticipationUpdate n =
  ParticipationUpdate
    <$> arbitraryReducedMaybe n -- participationUpdateAcquiredAt :: Maybe Date
    <*> arbitraryReducedMaybe n -- participationUpdateBoardAppointment :: Maybe Bool
    <*> arbitraryReducedMaybe n -- participationUpdateCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- participationUpdateControlAgreement :: Maybe Bool
    <*> arbitraryReducedMaybe n -- participationUpdateLegalForm :: Maybe Text
    <*> arbitraryReducedMaybe n -- participationUpdateOwnershipPct :: Maybe Text
    <*> arbitraryReducedMaybe n -- participationUpdatePurposeVehicle :: Maybe Bool
    <*> arbitraryReducedMaybe n -- participationUpdateVotingMajority :: Maybe Bool
  
instance Arbitrary PayGapExportResponse where
  arbitrary = sized genPayGapExportResponse

genPayGapExportResponse :: Int -> Gen PayGapExportResponse
genPayGapExportResponse n =
  PayGapExportResponse
    <$> arbitrary -- payGapExportResponseCsvContent :: Text
    <*> arbitrary -- payGapExportResponseFilename :: Text
  
instance Arbitrary PayGapInfoResponse where
  arbitrary = sized genPayGapInfoResponse

genPayGapInfoResponse :: Int -> Gen PayGapInfoResponse
genPayGapInfoResponse n =
  PayGapInfoResponse
    <$> arbitrary -- payGapInfoResponseEmployeeId :: Text
    <*> arbitrary -- payGapInfoResponseFirstName :: Text
    <*> arbitraryReducedMaybe n -- payGapInfoResponseGender :: Maybe Text
    <*> arbitraryReducedMaybe n -- payGapInfoResponseGroupMedianHourly :: Maybe Double
    <*> arbitraryReducedMaybe n -- payGapInfoResponseGroupMedianMonthly :: Maybe Double
    <*> arbitrary -- payGapInfoResponseGroupSize :: Int
    <*> arbitrary -- payGapInfoResponseJobTitle :: Text
    <*> arbitrary -- payGapInfoResponseLastName :: Text
    <*> arbitraryReducedMaybe n -- payGapInfoResponseOverallMedianHourly :: Maybe Double
    <*> arbitraryReducedMaybe n -- payGapInfoResponseOwnHourlyGross :: Maybe Double
    <*> arbitraryReducedMaybe n -- payGapInfoResponseOwnMonthlyGross :: Maybe Double
  
instance Arbitrary PayGapReport where
  arbitrary = sized genPayGapReport

genPayGapReport :: Int -> Gen PayGapReport
genPayGapReport n =
  PayGapReport
    <$> arbitraryReduced n -- payGapReportByJobTitle :: [JobTitleGap]
    <*> arbitrary -- payGapReportDiverseCount :: Int
    <*> arbitrary -- payGapReportEmployeeCount :: Int
    <*> arbitrary -- payGapReportFemaleCount :: Int
    <*> arbitrary -- payGapReportMaleCount :: Int
    <*> arbitrary -- payGapReportMeanGapPct :: Double
    <*> arbitrary -- payGapReportMedianGapPct :: Double
    <*> arbitraryReduced n -- payGapReportQuartiles :: [QuartileBand]
  
instance Arbitrary Payment where
  arbitrary = sized genPayment

genPayment :: Int -> Gen Payment
genPayment n =
  Payment
    <$> arbitraryReducedMaybe n -- paymentAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentAttachment :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- paymentCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentMetadata :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- paymentMethod :: Maybe PaymentMethod
    <*> arbitraryReducedMaybe n -- paymentPaymentDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- paymentReference :: Maybe Text
  
instance Arbitrary PaymentCondition where
  arbitrary = sized genPaymentCondition

genPaymentCondition :: Int -> Gen PaymentCondition
genPaymentCondition n =
  PaymentCondition
    <$> arbitrary -- paymentConditionDiscountDays :: Int
    <*> arbitrary -- paymentConditionDiscountPercentage :: Double
    <*> arbitrary -- paymentConditionId :: Text
    <*> arbitrary -- paymentConditionName :: Text
    <*> arbitrary -- paymentConditionPaymentTermDays :: Int
  
instance Arbitrary PaymentCreate where
  arbitrary = sized genPaymentCreate

genPaymentCreate :: Int -> Gen PaymentCreate
genPaymentCreate n =
  PaymentCreate
    <$> arbitraryReducedMaybe n -- paymentCreateAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentCreateAttachment :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- paymentCreateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentCreateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentCreateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentCreateMetadata :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- paymentCreateMethod :: Maybe PaymentMethod
    <*> arbitraryReducedMaybe n -- paymentCreatePaymentDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- paymentCreateReference :: Maybe Text
  
instance Arbitrary PaymentGateway where
  arbitrary = sized genPaymentGateway

genPaymentGateway :: Int -> Gen PaymentGateway
genPaymentGateway n =
  PaymentGateway
    <$> arbitraryReduced n -- paymentGatewayConfig :: AnyType
    <*> arbitraryReduced n -- paymentGatewayCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- paymentGatewayDeletedAt :: Maybe DateTime
    <*> arbitrary -- paymentGatewayEnabled :: Bool
    <*> arbitrary -- paymentGatewayGatewayId :: Text
    <*> arbitraryReduced n -- paymentGatewayGatewayType :: GatewayType
    <*> arbitrary -- paymentGatewayLabel :: Text
    <*> arbitrary -- paymentGatewayTenantId :: Text
    <*> arbitraryReducedMaybe n -- paymentGatewayUpdatedAt :: Maybe DateTime
  
instance Arbitrary PaymentGatewayCreate where
  arbitrary = sized genPaymentGatewayCreate

genPaymentGatewayCreate :: Int -> Gen PaymentGatewayCreate
genPaymentGatewayCreate n =
  PaymentGatewayCreate
    <$> arbitraryReduced n -- paymentGatewayCreateConfig :: AnyType
    <*> arbitraryReduced n -- paymentGatewayCreateCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- paymentGatewayCreateDeletedAt :: Maybe DateTime
    <*> arbitrary -- paymentGatewayCreateEnabled :: Bool
    <*> arbitraryReduced n -- paymentGatewayCreateGatewayType :: GatewayType
    <*> arbitrary -- paymentGatewayCreateLabel :: Text
    <*> arbitraryReducedMaybe n -- paymentGatewayCreateUpdatedAt :: Maybe DateTime
  
instance Arbitrary PaymentGatewayUpdate where
  arbitrary = sized genPaymentGatewayUpdate

genPaymentGatewayUpdate :: Int -> Gen PaymentGatewayUpdate
genPaymentGatewayUpdate n =
  PaymentGatewayUpdate
    <$> arbitraryReducedMaybe n -- paymentGatewayUpdateConfig :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- paymentGatewayUpdateCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- paymentGatewayUpdateDeletedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- paymentGatewayUpdateEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- paymentGatewayUpdateGatewayType :: Maybe GatewayType
    <*> arbitraryReducedMaybe n -- paymentGatewayUpdateLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- paymentGatewayUpdateUpdatedAt :: Maybe DateTime
  
instance Arbitrary PayrollAutopayPayload where
  arbitrary = sized genPayrollAutopayPayload

genPayrollAutopayPayload :: Int -> Gen PayrollAutopayPayload
genPayrollAutopayPayload n =
  PayrollAutopayPayload
    <$> arbitraryReducedMaybe n -- payrollAutopayPayloadDebtorBic :: Maybe Text
    <*> arbitraryReducedMaybe n -- payrollAutopayPayloadDebtorIban :: Maybe Text
    <*> arbitraryReducedMaybe n -- payrollAutopayPayloadDebtorName :: Maybe Text
    <*> arbitraryReducedMaybe n -- payrollAutopayPayloadExecutionDate :: Maybe Date
  
instance Arbitrary PayrollCreatePayload where
  arbitrary = sized genPayrollCreatePayload

genPayrollCreatePayload :: Int -> Gen PayrollCreatePayload
genPayrollCreatePayload n =
  PayrollCreatePayload
    <$> arbitrary -- payrollCreatePayloadEmployeeIds :: [Text]
    <*> arbitraryReducedMaybe n -- payrollCreatePayloadExtraPayments :: Maybe [ExtraPayment]
    <*> arbitrary -- payrollCreatePayloadMonth :: Int
    <*> arbitrary -- payrollCreatePayloadYear :: Int
  
instance Arbitrary PayrollEntryApi where
  arbitrary = sized genPayrollEntryApi

genPayrollEntryApi :: Int -> Gen PayrollEntryApi
genPayrollEntryApi n =
  PayrollEntryApi
    <$> arbitrary -- payrollEntryApiAvEmployee :: Text
    <*> arbitrary -- payrollEntryApiAvEmployer :: Text
    <*> arbitrary -- payrollEntryApiChurchTaxAmount :: Text
    <*> arbitraryReducedMaybe n -- payrollEntryApiEmployee :: Maybe Employee
    <*> arbitrary -- payrollEntryApiEmployeeId :: Text
    <*> arbitrary -- payrollEntryApiEntryId :: Text
    <*> arbitraryReducedMaybe n -- payrollEntryApiExtraPaymentReason :: Maybe Text
    <*> arbitrary -- payrollEntryApiExtraPayments :: Text
    <*> arbitrary -- payrollEntryApiGrossSalary :: Text
    <*> arbitrary -- payrollEntryApiKvEmployee :: Text
    <*> arbitrary -- payrollEntryApiKvEmployer :: Text
    <*> arbitrary -- payrollEntryApiLohnsteuer :: Text
    <*> arbitrary -- payrollEntryApiNetSalary :: Text
    <*> arbitraryReducedMaybe n -- payrollEntryApiNotes :: Maybe Text
    <*> arbitrary -- payrollEntryApiPvEmployee :: Text
    <*> arbitrary -- payrollEntryApiPvEmployer :: Text
    <*> arbitrary -- payrollEntryApiRunId :: Text
    <*> arbitrary -- payrollEntryApiRvEmployee :: Text
    <*> arbitrary -- payrollEntryApiRvEmployer :: Text
    <*> arbitrary -- payrollEntryApiSickDays :: Int
    <*> arbitrary -- payrollEntryApiSoli :: Text
    <*> arbitraryReduced n -- payrollEntryApiStatus :: PayrollRunStatus
    <*> arbitrary -- payrollEntryApiTotalDeductions :: Text
    <*> arbitrary -- payrollEntryApiTotalEmployerCost :: Text
    <*> arbitrary -- payrollEntryApiVacationDaysUsed :: Int
  
instance Arbitrary PayrollMonth where
  arbitrary = sized genPayrollMonth

genPayrollMonth :: Int -> Gen PayrollMonth
genPayrollMonth n =
  PayrollMonth
    <$> arbitrary -- payrollMonthGross :: Text
    <*> arbitrary -- payrollMonthMonth :: Int
    <*> arbitrary -- payrollMonthNet :: Text
  
instance Arbitrary PayrollPayPayload where
  arbitrary = sized genPayrollPayPayload

genPayrollPayPayload :: Int -> Gen PayrollPayPayload
genPayrollPayPayload n =
  PayrollPayPayload
    <$> arbitraryReduced n -- payrollPayPayloadPaymentDate :: Date
  
instance Arbitrary PayrollRunApi where
  arbitrary = sized genPayrollRunApi

genPayrollRunApi :: Int -> Gen PayrollRunApi
genPayrollRunApi n =
  PayrollRunApi
    <$> arbitraryReducedMaybe n -- payrollRunApiApprovedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- payrollRunApiApprovedBy :: Maybe Text
    <*> arbitraryReduced n -- payrollRunApiCreatedAt :: DateTime
    <*> arbitraryReduced n -- payrollRunApiEntries :: [PayrollEntryApi]
    <*> arbitrary -- payrollRunApiMonth :: Int
    <*> arbitraryReducedMaybe n -- payrollRunApiPaymentDate :: Maybe Date
    <*> arbitrary -- payrollRunApiPeriodLabel :: Text
    <*> arbitrary -- payrollRunApiRunId :: Text
    <*> arbitraryReduced n -- payrollRunApiStatus :: PayrollRunStatus
    <*> arbitrary -- payrollRunApiTenantId :: Text
    <*> arbitrary -- payrollRunApiTotalEmployeeCount :: Int
    <*> arbitrary -- payrollRunApiTotalEmployerCost :: Text
    <*> arbitrary -- payrollRunApiTotalGross :: Text
    <*> arbitrary -- payrollRunApiTotalNet :: Text
    <*> arbitrary -- payrollRunApiTotalSocialSecurity :: Text
    <*> arbitrary -- payrollRunApiTotalTaxes :: Text
    <*> arbitraryReducedMaybe n -- payrollRunApiUpdatedAt :: Maybe DateTime
    <*> arbitrary -- payrollRunApiYear :: Int
  
instance Arbitrary PayrollSummary where
  arbitrary = sized genPayrollSummary

genPayrollSummary :: Int -> Gen PayrollSummary
genPayrollSummary n =
  PayrollSummary
    <$> arbitrary -- payrollSummaryFirstName :: Text
    <*> arbitraryReducedMaybe n -- payrollSummaryHourlyGross :: Maybe Text
    <*> arbitrary -- payrollSummaryId :: Text
    <*> arbitrary -- payrollSummaryJobTitle :: Text
    <*> arbitrary -- payrollSummaryLastName :: Text
    <*> arbitraryReducedMaybe n -- payrollSummaryMonthlySalary :: Maybe Text
    <*> arbitraryReduced n -- payrollSummaryMonths :: [PayrollMonth]
    <*> arbitraryReducedMaybe n -- payrollSummaryWeeklyHours :: Maybe Text
    <*> arbitrary -- payrollSummaryYear :: Int
  
instance Arbitrary PayrollSummaryItem where
  arbitrary = sized genPayrollSummaryItem

genPayrollSummaryItem :: Int -> Gen PayrollSummaryItem
genPayrollSummaryItem n =
  PayrollSummaryItem
    <$> arbitrary -- payrollSummaryItemEmployeeCount :: Int
    <*> arbitrary -- payrollSummaryItemMonth :: Text
    <*> arbitraryReduced n -- payrollSummaryItemStatus :: PayrollRunStatus
    <*> arbitrary -- payrollSummaryItemTotalEmployerCost :: Text
    <*> arbitrary -- payrollSummaryItemTotalGross :: Text
    <*> arbitrary -- payrollSummaryItemTotalNet :: Text
    <*> arbitrary -- payrollSummaryItemYear :: Int
  
instance Arbitrary PeppolResponse where
  arbitrary = sized genPeppolResponse

genPeppolResponse :: Int -> Gen PeppolResponse
genPeppolResponse n =
  PeppolResponse
    <$> arbitrary -- peppolResponseContent :: Text
    <*> arbitrary -- peppolResponseContentType :: Text
    <*> arbitrary -- peppolResponseFilename :: Text
  
instance Arbitrary Plan where
  arbitrary = sized genPlan

genPlan :: Int -> Gen Plan
genPlan n =
  Plan
    <$> arbitraryReduced n -- planFeatures :: PlanFeatures
    <*> arbitrary -- planId :: Text
    <*> arbitraryReduced n -- planLimits :: PlanLimits
    <*> arbitrary -- planName :: Text
    <*> arbitrary -- planPriceEur :: Double
  
instance Arbitrary PlanFeatures where
  arbitrary = sized genPlanFeatures

genPlanFeatures :: Int -> Gen PlanFeatures
genPlanFeatures n =
  PlanFeatures
    <$> arbitrary -- planFeaturesConnectors :: Bool
    <*> arbitrary -- planFeaturesErp :: Bool
    <*> arbitrary -- planFeaturesFancyReports :: Bool
    <*> arbitrary -- planFeaturesTaxAutomations :: Bool
  
instance Arbitrary PlanLimits where
  arbitrary = sized genPlanLimits

genPlanLimits :: Int -> Gen PlanLimits
genPlanLimits n =
  PlanLimits
    <$> arbitrary -- planLimitsMaxConnectors :: Int
    <*> arbitrary -- planLimitsMaxInvoicesPerMonth :: Integer
    <*> arbitrary -- planLimitsMaxUsers :: Int
    <*> arbitraryReducedMaybe n -- planLimitsMetered :: Maybe (Map.Map String Integer)
    <*> arbitrary -- planLimitsPaidConnectors :: [Text]
  
instance Arbitrary PlatformInfo where
  arbitrary = sized genPlatformInfo

genPlatformInfo :: Int -> Gen PlatformInfo
genPlatformInfo n =
  PlatformInfo
    <$> arbitrary -- platformInfoAuthor :: Text
    <*> arbitraryReduced n -- platformInfoChangelog :: [ChangelogEntry]
    <*> arbitrary -- platformInfoConfigFieldNames :: [Text]
    <*> arbitraryReduced n -- platformInfoConfigFields :: [ConfigFieldInfo]
    <*> arbitrary -- platformInfoDisplayName :: Text
    <*> arbitrary -- platformInfoPlatform :: Text
    <*> arbitraryReduced n -- platformInfoPricing :: PluginPricing
    <*> arbitrary -- platformInfoSupportedEntities :: [Text]
    <*> arbitrary -- platformInfoSupportsExport :: Bool
    <*> arbitrary -- platformInfoSupportsImport :: Bool
    <*> arbitrary -- platformInfoSupportsOauth :: Bool
    <*> arbitrary -- platformInfoVersion :: Text
  
instance Arbitrary PlausibilityCheck where
  arbitrary = sized genPlausibilityCheck

genPlausibilityCheck :: Int -> Gen PlausibilityCheck
genPlausibilityCheck n =
  PlausibilityCheck
    <$> arbitrary -- plausibilityCheckDetail :: Text
    <*> arbitrary -- plausibilityCheckId :: Text
    <*> arbitrary -- plausibilityCheckName :: Text
    <*> arbitraryReduced n -- plausibilityCheckSeverity :: Severity
    <*> arbitraryReduced n -- plausibilityCheckStatus :: CheckStatus
  
instance Arbitrary PlausibilityReport where
  arbitrary = sized genPlausibilityReport

genPlausibilityReport :: Int -> Gen PlausibilityReport
genPlausibilityReport n =
  PlausibilityReport
    <$> arbitraryReduced n -- plausibilityReportChecks :: [PlausibilityCheck]
    <*> arbitrary -- plausibilityReportGeneratedAt :: Text
    <*> arbitraryReduced n -- plausibilityReportSummary :: PlausibilitySummary
  
instance Arbitrary PlausibilitySummary where
  arbitrary = sized genPlausibilitySummary

genPlausibilitySummary :: Int -> Gen PlausibilitySummary
genPlausibilitySummary n =
  PlausibilitySummary
    <$> arbitrary -- plausibilitySummaryErrors :: Int
    <*> arbitraryReduced n -- plausibilitySummaryOverallStatus :: CheckStatus
    <*> arbitrary -- plausibilitySummaryPassed :: Int
    <*> arbitrary -- plausibilitySummaryTotalChecks :: Int
    <*> arbitrary -- plausibilitySummaryWarnings :: Int
  
instance Arbitrary PluginError where
  arbitrary = sized genPluginError

genPluginError :: Int -> Gen PluginError
genPluginError n =
  PluginError
    <$> arbitraryReduced n -- pluginErrorBadRequest :: [A.Value]
    <*> arbitraryReduced n -- pluginErrorNotFound :: [A.Value]
    <*> arbitraryReduced n -- pluginErrorUnauthorized :: [A.Value]
    <*> arbitraryReduced n -- pluginErrorInternalError :: [A.Value]
    <*> arbitraryReduced n -- pluginErrorDatabaseError :: [A.Value]
    <*> arbitraryReduced n -- pluginErrorValidationError :: [A.Value]
    <*> arbitrary -- pluginErrorNotImplemented :: Text
  
instance Arbitrary PluginErrorOneOf where
  arbitrary = sized genPluginErrorOneOf

genPluginErrorOneOf :: Int -> Gen PluginErrorOneOf
genPluginErrorOneOf n =
  PluginErrorOneOf
    <$> arbitraryReduced n -- pluginErrorOneOfBadRequest :: [A.Value]
  
instance Arbitrary PluginErrorOneOf1 where
  arbitrary = sized genPluginErrorOneOf1

genPluginErrorOneOf1 :: Int -> Gen PluginErrorOneOf1
genPluginErrorOneOf1 n =
  PluginErrorOneOf1
    <$> arbitraryReduced n -- pluginErrorOneOf1NotFound :: [A.Value]
  
instance Arbitrary PluginErrorOneOf2 where
  arbitrary = sized genPluginErrorOneOf2

genPluginErrorOneOf2 :: Int -> Gen PluginErrorOneOf2
genPluginErrorOneOf2 n =
  PluginErrorOneOf2
    <$> arbitraryReduced n -- pluginErrorOneOf2Unauthorized :: [A.Value]
  
instance Arbitrary PluginErrorOneOf3 where
  arbitrary = sized genPluginErrorOneOf3

genPluginErrorOneOf3 :: Int -> Gen PluginErrorOneOf3
genPluginErrorOneOf3 n =
  PluginErrorOneOf3
    <$> arbitraryReduced n -- pluginErrorOneOf3InternalError :: [A.Value]
  
instance Arbitrary PluginErrorOneOf4 where
  arbitrary = sized genPluginErrorOneOf4

genPluginErrorOneOf4 :: Int -> Gen PluginErrorOneOf4
genPluginErrorOneOf4 n =
  PluginErrorOneOf4
    <$> arbitraryReduced n -- pluginErrorOneOf4DatabaseError :: [A.Value]
  
instance Arbitrary PluginErrorOneOf5 where
  arbitrary = sized genPluginErrorOneOf5

genPluginErrorOneOf5 :: Int -> Gen PluginErrorOneOf5
genPluginErrorOneOf5 n =
  PluginErrorOneOf5
    <$> arbitraryReduced n -- pluginErrorOneOf5ValidationError :: [A.Value]
  
instance Arbitrary PluginErrorOneOf6 where
  arbitrary = sized genPluginErrorOneOf6

genPluginErrorOneOf6 :: Int -> Gen PluginErrorOneOf6
genPluginErrorOneOf6 n =
  PluginErrorOneOf6
    <$> arbitrary -- pluginErrorOneOf6NotImplemented :: Text
  
instance Arbitrary PluginPricing where
  arbitrary = sized genPluginPricing

genPluginPricing :: Int -> Gen PluginPricing
genPluginPricing n =
  PluginPricing
    <$> arbitrary -- pluginPricingType :: E'Type2
    <*> arbitrary -- pluginPricingPrice :: Double
    <*> arbitrary -- pluginPricingPricePerMonth :: Double
  
instance Arbitrary PluginPricingOneOf where
  arbitrary = sized genPluginPricingOneOf

genPluginPricingOneOf :: Int -> Gen PluginPricingOneOf
genPluginPricingOneOf n =
  PluginPricingOneOf
    <$> arbitrary -- pluginPricingOneOfType :: E'Type8
  
instance Arbitrary PluginPricingOneOf1 where
  arbitrary = sized genPluginPricingOneOf1

genPluginPricingOneOf1 :: Int -> Gen PluginPricingOneOf1
genPluginPricingOneOf1 n =
  PluginPricingOneOf1
    <$> arbitrary -- pluginPricingOneOf1Price :: Double
    <*> arbitrary -- pluginPricingOneOf1Type :: E'Type9
  
instance Arbitrary PluginPricingOneOf2 where
  arbitrary = sized genPluginPricingOneOf2

genPluginPricingOneOf2 :: Int -> Gen PluginPricingOneOf2
genPluginPricingOneOf2 n =
  PluginPricingOneOf2
    <$> arbitrary -- pluginPricingOneOf2PricePerMonth :: Double
    <*> arbitrary -- pluginPricingOneOf2Type :: E'Type10
  
instance Arbitrary PnLItem where
  arbitrary = sized genPnLItem

genPnLItem :: Int -> Gen PnLItem
genPnLItem n =
  PnLItem
    <$> arbitrary -- pnLItemAccount :: Text
    <*> arbitrary -- pnLItemAccountName :: Text
    <*> arbitrary -- pnLItemAmount :: Text
  
instance Arbitrary PosRegister where
  arbitrary = sized genPosRegister

genPosRegister :: Int -> Gen PosRegister
genPosRegister n =
  PosRegister
    <$> arbitrary -- posRegisterName :: Text
    <*> arbitraryReducedMaybe n -- posRegisterStatus :: Maybe PosRegisterStatus
  
instance Arbitrary PosRegisterCreate where
  arbitrary = sized genPosRegisterCreate

genPosRegisterCreate :: Int -> Gen PosRegisterCreate
genPosRegisterCreate n =
  PosRegisterCreate
    <$> arbitrary -- posRegisterCreateName :: Text
    <*> arbitraryReducedMaybe n -- posRegisterCreateStatus :: Maybe PosRegisterStatus
  
instance Arbitrary PosTable where
  arbitrary = sized genPosTable

genPosTable :: Int -> Gen PosTable
genPosTable n =
  PosTable
    <$> arbitraryReducedMaybe n -- posTableCurrentOrderNumber :: Maybe Text
    <*> arbitrary -- posTableName :: Text
    <*> arbitraryReducedMaybe n -- posTableStatus :: Maybe PosTableStatus
  
instance Arbitrary PosTableCreate where
  arbitrary = sized genPosTableCreate

genPosTableCreate :: Int -> Gen PosTableCreate
genPosTableCreate n =
  PosTableCreate
    <$> arbitraryReducedMaybe n -- posTableCreateCurrentOrderNumber :: Maybe Text
    <*> arbitrary -- posTableCreateName :: Text
    <*> arbitraryReducedMaybe n -- posTableCreateStatus :: Maybe PosTableStatus
  
instance Arbitrary PostingCategory where
  arbitrary = sized genPostingCategory

genPostingCategory :: Int -> Gen PostingCategory
genPostingCategory n =
  PostingCategory
    <$> arbitraryReducedMaybe n -- postingCategoryAccountNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryAccountNumberSkr03 :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryAccountNumberSkr04 :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryAccountNumberSkr49 :: Maybe Text
    <*> arbitrary -- postingCategoryCategoryId :: Text
    <*> arbitrary -- postingCategoryDefaultVatRate :: Int
    <*> arbitraryReducedMaybe n -- postingCategoryDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryEksCategory :: Maybe Text
    <*> arbitrary -- postingCategoryIsActive :: Bool
    <*> arbitrary -- postingCategoryIsSystem :: Bool
    <*> arbitrary -- postingCategoryName :: Text
    <*> arbitrary -- postingCategorySkrVersion :: Text
    <*> arbitrary -- postingCategoryType :: Text
  
instance Arbitrary PostingCategoryCreate where
  arbitrary = sized genPostingCategoryCreate

genPostingCategoryCreate :: Int -> Gen PostingCategoryCreate
genPostingCategoryCreate n =
  PostingCategoryCreate
    <$> arbitraryReducedMaybe n -- postingCategoryCreateAccountNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryCreateAccountNumberSkr03 :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryCreateAccountNumberSkr04 :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryCreateAccountNumberSkr49 :: Maybe Text
    <*> arbitraryReduced n -- postingCategoryCreateCategoryType :: PostingCategoryType
    <*> arbitraryReduced n -- postingCategoryCreateCreatedAt :: DateTime
    <*> arbitrary -- postingCategoryCreateDefaultVatRate :: Int
    <*> arbitraryReducedMaybe n -- postingCategoryCreateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryCreateEksCategory :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryCreateEuVatLine :: Maybe Int
    <*> arbitrary -- postingCategoryCreateInputVatPercentage :: Text
    <*> arbitrary -- postingCategoryCreateIsActive :: Bool
    <*> arbitrary -- postingCategoryCreateIsSystem :: Bool
    <*> arbitrary -- postingCategoryCreateName :: Text
    <*> arbitrary -- postingCategoryCreateSkrVersion :: Text
    <*> arbitraryReducedMaybe n -- postingCategoryCreateUpdatedAt :: Maybe DateTime
    <*> arbitrary -- postingCategoryCreateUserModifiedSkr03 :: Bool
    <*> arbitrary -- postingCategoryCreateUserModifiedSkr04 :: Bool
  
instance Arbitrary PostingCategoryUpdate where
  arbitrary = sized genPostingCategoryUpdate

genPostingCategoryUpdate :: Int -> Gen PostingCategoryUpdate
genPostingCategoryUpdate n =
  PostingCategoryUpdate
    <$> arbitraryReducedMaybe n -- postingCategoryUpdateAccountNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateAccountNumberSkr03 :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateAccountNumberSkr04 :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateAccountNumberSkr49 :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateCategoryType :: Maybe PostingCategoryType
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateDefaultVatRate :: Maybe Int
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateEksCategory :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateEuVatLine :: Maybe Int
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateInputVatPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateIsSystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateSkrVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateUserModifiedSkr03 :: Maybe Bool
    <*> arbitraryReducedMaybe n -- postingCategoryUpdateUserModifiedSkr04 :: Maybe Bool
  
instance Arbitrary PriceTier where
  arbitrary = sized genPriceTier

genPriceTier :: Int -> Gen PriceTier
genPriceTier n =
  PriceTier
    <$> arbitraryReducedMaybe n -- priceTierCustomerGroupId :: Maybe Text
    <*> arbitraryReducedMaybe n -- priceTierMinQuantity :: Maybe Integer
    <*> arbitrary -- priceTierProductId :: Text
    <*> arbitrary -- priceTierUnitPrice :: Text
  
instance Arbitrary PriceTierCreate where
  arbitrary = sized genPriceTierCreate

genPriceTierCreate :: Int -> Gen PriceTierCreate
genPriceTierCreate n =
  PriceTierCreate
    <$> arbitraryReducedMaybe n -- priceTierCreateCustomerGroupId :: Maybe Text
    <*> arbitraryReducedMaybe n -- priceTierCreateMinQuantity :: Maybe Integer
    <*> arbitrary -- priceTierCreateProductId :: Text
    <*> arbitrary -- priceTierCreateUnitPrice :: Text
  
instance Arbitrary PriceTierUpdate where
  arbitrary = sized genPriceTierUpdate

genPriceTierUpdate :: Int -> Gen PriceTierUpdate
genPriceTierUpdate n =
  PriceTierUpdate
    <$> arbitraryReducedMaybe n -- priceTierUpdateCustomerGroupId :: Maybe Text
    <*> arbitraryReducedMaybe n -- priceTierUpdateMinQuantity :: Maybe Integer
    <*> arbitraryReducedMaybe n -- priceTierUpdateProductId :: Maybe Text
    <*> arbitrary -- priceTierUpdateUnitPrice :: Text
  
instance Arbitrary PrintDeliveryNoteResponse where
  arbitrary = sized genPrintDeliveryNoteResponse

genPrintDeliveryNoteResponse :: Int -> Gen PrintDeliveryNoteResponse
genPrintDeliveryNoteResponse n =
  PrintDeliveryNoteResponse
    <$> arbitrary -- printDeliveryNoteResponseMessage :: Text
    <*> arbitraryReducedMaybe n -- printDeliveryNoteResponsePdfUrl :: Maybe Text
    <*> arbitrary -- printDeliveryNoteResponseSuccess :: Bool
  
instance Arbitrary PrintLabelResponse where
  arbitrary = sized genPrintLabelResponse

genPrintLabelResponse :: Int -> Gen PrintLabelResponse
genPrintLabelResponse n =
  PrintLabelResponse
    <$> arbitraryReducedMaybe n -- printLabelResponseLabelUrl :: Maybe Text
    <*> arbitrary -- printLabelResponseMessage :: Text
    <*> arbitraryReducedMaybe n -- printLabelResponseSscc :: Maybe Text
    <*> arbitrary -- printLabelResponseSuccess :: Bool
    <*> arbitraryReducedMaybe n -- printLabelResponseTrackingNumber :: Maybe Text
  
instance Arbitrary Product where
  arbitrary = sized genProduct

genProduct :: Int -> Gen Product
genProduct n =
  Product
    <$> arbitraryReducedMaybe n -- productAvailability :: Maybe Text
    <*> arbitraryReducedMaybe n -- productBarcode :: Maybe Text
    <*> arbitraryReducedMaybe n -- productBrand :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCategoryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCondition :: Maybe Text
    <*> arbitraryReducedMaybe n -- productDefaultLedgerAccount :: Maybe Text
    <*> arbitraryReducedMaybe n -- productDefaultPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productDefaultPriceFormulaId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productDefaultTaxRate :: Maybe Text
    <*> arbitraryReducedMaybe n -- productDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- productGtin :: Maybe Text
    <*> arbitraryReducedMaybe n -- productHeight :: Maybe Text
    <*> arbitraryReducedMaybe n -- productImageLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- productImages :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productIsTaxable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productLength :: Maybe Text
    <*> arbitraryReducedMaybe n -- productLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- productMaxStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productMinStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productMpn :: Maybe Text
    <*> arbitrary -- productName :: Text
    <*> arbitraryReducedMaybe n -- productPackageHeight :: Maybe Text
    <*> arbitraryReducedMaybe n -- productPackageLength :: Maybe Text
    <*> arbitraryReducedMaybe n -- productPackageWeightUnit :: Maybe Text
    <*> arbitraryReducedMaybe n -- productPackageWeightValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- productPackageWidth :: Maybe Text
    <*> arbitrary -- productProductCode :: Text
    <*> arbitraryReducedMaybe n -- productProductType :: Maybe Text
    <*> arbitraryReducedMaybe n -- productPurchasePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productReorderQuantity :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productSalePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productShippingPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productShippingRequiresInsurance :: Maybe Bool
    <*> arbitrary -- productSku :: Text
    <*> arbitraryReducedMaybe n -- productStockQuantity :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productTags :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productTaxPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productTrackBatch :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productTrackSerial :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productUnit :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productWeightUnit :: Maybe Text
    <*> arbitraryReducedMaybe n -- productWeightValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- productWidth :: Maybe Text
  
instance Arbitrary ProductAttribute where
  arbitrary = sized genProductAttribute

genProductAttribute :: Int -> Gen ProductAttribute
genProductAttribute n =
  ProductAttribute
    <$> arbitraryReducedMaybe n -- productAttributeIsFilterable :: Maybe Bool
    <*> arbitrary -- productAttributeName :: Text
    <*> arbitraryReducedMaybe n -- productAttributePosition :: Maybe Int
    <*> arbitrary -- productAttributeProductId :: Text
    <*> arbitraryReducedMaybe n -- productAttributeUnit :: Maybe Text
    <*> arbitrary -- productAttributeValue :: Text
  
instance Arbitrary ProductAttributeCreate where
  arbitrary = sized genProductAttributeCreate

genProductAttributeCreate :: Int -> Gen ProductAttributeCreate
genProductAttributeCreate n =
  ProductAttributeCreate
    <$> arbitraryReducedMaybe n -- productAttributeCreateIsFilterable :: Maybe Bool
    <*> arbitrary -- productAttributeCreateName :: Text
    <*> arbitraryReducedMaybe n -- productAttributeCreatePosition :: Maybe Int
    <*> arbitrary -- productAttributeCreateProductId :: Text
    <*> arbitraryReducedMaybe n -- productAttributeCreateUnit :: Maybe Text
    <*> arbitrary -- productAttributeCreateValue :: Text
  
instance Arbitrary ProductAttributeUpdate where
  arbitrary = sized genProductAttributeUpdate

genProductAttributeUpdate :: Int -> Gen ProductAttributeUpdate
genProductAttributeUpdate n =
  ProductAttributeUpdate
    <$> arbitraryReducedMaybe n -- productAttributeUpdateIsFilterable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productAttributeUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- productAttributeUpdatePosition :: Maybe Int
    <*> arbitraryReducedMaybe n -- productAttributeUpdateProductId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productAttributeUpdateUnit :: Maybe Text
    <*> arbitraryReducedMaybe n -- productAttributeUpdateValue :: Maybe Text
  
instance Arbitrary ProductCategory where
  arbitrary = sized genProductCategory

genProductCategory :: Int -> Gen ProductCategory
genProductCategory n =
  ProductCategory
    <$> arbitraryReducedMaybe n -- productCategoryDescription :: Maybe Text
    <*> arbitrary -- productCategoryName :: Text
    <*> arbitraryReducedMaybe n -- productCategoryParentCategoryId :: Maybe Text
    <*> arbitrary -- productCategorySortOrder :: Int
  
instance Arbitrary ProductCategoryCreate where
  arbitrary = sized genProductCategoryCreate

genProductCategoryCreate :: Int -> Gen ProductCategoryCreate
genProductCategoryCreate n =
  ProductCategoryCreate
    <$> arbitraryReducedMaybe n -- productCategoryCreateDescription :: Maybe Text
    <*> arbitrary -- productCategoryCreateName :: Text
    <*> arbitraryReducedMaybe n -- productCategoryCreateParentCategoryId :: Maybe Text
    <*> arbitrary -- productCategoryCreateSortOrder :: Int
  
instance Arbitrary ProductCategoryUpdate where
  arbitrary = sized genProductCategoryUpdate

genProductCategoryUpdate :: Int -> Gen ProductCategoryUpdate
genProductCategoryUpdate n =
  ProductCategoryUpdate
    <$> arbitraryReducedMaybe n -- productCategoryUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCategoryUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCategoryUpdateParentCategoryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCategoryUpdateSortOrder :: Maybe Int
  
instance Arbitrary ProductCreate where
  arbitrary = sized genProductCreate

genProductCreate :: Int -> Gen ProductCreate
genProductCreate n =
  ProductCreate
    <$> arbitraryReducedMaybe n -- productCreateAvailability :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateBarcode :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateBrand :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateCategoryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateCondition :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateDefaultLedgerAccount :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateDefaultPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateDefaultPriceFormulaId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateDefaultTaxRate :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateGtin :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateHeight :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateImageLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateImages :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productCreateIsTaxable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productCreateLength :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateMaxStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productCreateMinStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productCreateMpn :: Maybe Text
    <*> arbitrary -- productCreateName :: Text
    <*> arbitraryReducedMaybe n -- productCreatePackageHeight :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreatePackageLength :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreatePackageWeightUnit :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreatePackageWeightValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreatePackageWidth :: Maybe Text
    <*> arbitrary -- productCreateProductCode :: Text
    <*> arbitraryReducedMaybe n -- productCreateProductType :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreatePurchasePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateReorderQuantity :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productCreateSalePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateShippingPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateShippingRequiresInsurance :: Maybe Bool
    <*> arbitrary -- productCreateSku :: Text
    <*> arbitraryReducedMaybe n -- productCreateStockQuantity :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productCreateTags :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productCreateTaxPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateTrackBatch :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productCreateTrackSerial :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productCreateUnit :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productCreateWeightUnit :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateWeightValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- productCreateWidth :: Maybe Text
  
instance Arbitrary ProductStock where
  arbitrary = sized genProductStock

genProductStock :: Int -> Gen ProductStock
genProductStock n =
  ProductStock
    <$> arbitrary -- productStockName :: Text
    <*> arbitrary -- productStockProductId :: Text
    <*> arbitrary -- productStockSku :: Text
    <*> arbitraryReducedMaybe n -- productStockStockQuantity :: Maybe Integer
  
instance Arbitrary ProductUpdate where
  arbitrary = sized genProductUpdate

genProductUpdate :: Int -> Gen ProductUpdate
genProductUpdate n =
  ProductUpdate
    <$> arbitraryReducedMaybe n -- productUpdateAvailability :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateBarcode :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateBrand :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateCategoryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateCondition :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateDefaultLedgerAccount :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateDefaultPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateDefaultPriceFormulaId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateDefaultTaxRate :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateGtin :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateHeight :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateImageLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateImages :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productUpdateIsTaxable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productUpdateLength :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateMaxStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productUpdateMinStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productUpdateMpn :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdatePackageHeight :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdatePackageLength :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdatePackageWeightUnit :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdatePackageWeightValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdatePackageWidth :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateProductCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateProductType :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdatePurchasePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateReorderQuantity :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productUpdateSalePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateShippingPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateShippingRequiresInsurance :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productUpdateSku :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateStockQuantity :: Maybe Integer
    <*> arbitraryReducedMaybe n -- productUpdateTags :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productUpdateTaxPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateTrackBatch :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productUpdateTrackSerial :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productUpdateUnit :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productUpdateWeightUnit :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateWeightValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- productUpdateWidth :: Maybe Text
  
instance Arbitrary ProductVariant where
  arbitrary = sized genProductVariant

genProductVariant :: Int -> Gen ProductVariant
genProductVariant n =
  ProductVariant
    <$> arbitraryReducedMaybe n -- productVariantBarcode :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantImageLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productVariantName :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantOptionValues :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productVariantPrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantPriceDelta :: Maybe Text
    <*> arbitrary -- productVariantProductId :: Text
    <*> arbitrary -- productVariantSku :: Text
    <*> arbitraryReducedMaybe n -- productVariantStockQuantity :: Maybe Integer
  
instance Arbitrary ProductVariantCreate where
  arbitrary = sized genProductVariantCreate

genProductVariantCreate :: Int -> Gen ProductVariantCreate
genProductVariantCreate n =
  ProductVariantCreate
    <$> arbitraryReducedMaybe n -- productVariantCreateBarcode :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantCreateImageLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantCreateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productVariantCreateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantCreateOptionValues :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productVariantCreatePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantCreatePriceDelta :: Maybe Text
    <*> arbitrary -- productVariantCreateProductId :: Text
    <*> arbitrary -- productVariantCreateSku :: Text
    <*> arbitraryReducedMaybe n -- productVariantCreateStockQuantity :: Maybe Integer
  
instance Arbitrary ProductVariantUpdate where
  arbitrary = sized genProductVariantUpdate

genProductVariantUpdate :: Int -> Gen ProductVariantUpdate
genProductVariantUpdate n =
  ProductVariantUpdate
    <$> arbitraryReducedMaybe n -- productVariantUpdateBarcode :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantUpdateImageLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantUpdateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- productVariantUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantUpdateOptionValues :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productVariantUpdatePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantUpdatePriceDelta :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantUpdateProductId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantUpdateSku :: Maybe Text
    <*> arbitraryReducedMaybe n -- productVariantUpdateStockQuantity :: Maybe Integer
  
instance Arbitrary ProductionOrder where
  arbitrary = sized genProductionOrder

genProductionOrder :: Int -> Gen ProductionOrder
genProductionOrder n =
  ProductionOrder
    <$> arbitraryReducedMaybe n -- productionOrderBomId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productionOrderComponents :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- productionOrderEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- productionOrderNotes :: Maybe Text
    <*> arbitrary -- productionOrderOrderNumber :: Text
    <*> arbitrary -- productionOrderProductId :: Text
    <*> arbitrary -- productionOrderQuantity :: Integer
    <*> arbitraryReducedMaybe n -- productionOrderSourceWarehouseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- productionOrderStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- productionOrderStatus :: Maybe ProductionOrderStatus
    <*> arbitraryReducedMaybe n -- productionOrderTargetWarehouseId :: Maybe Text
  
instance Arbitrary ProductionOrderCosting where
  arbitrary = sized genProductionOrderCosting

genProductionOrderCosting :: Int -> Gen ProductionOrderCosting
genProductionOrderCosting n =
  ProductionOrderCosting
    <$> arbitrary -- productionOrderCostingCostPerUnit :: Text
    <*> arbitrary -- productionOrderCostingCostSource :: Text
    <*> arbitraryReduced n -- productionOrderCostingLines :: [CostingLine]
    <*> arbitraryReducedMaybe n -- productionOrderCostingMarginPerUnit :: Maybe Text
    <*> arbitraryReducedMaybe n -- productionOrderCostingMarginPercent :: Maybe Text
    <*> arbitrary -- productionOrderCostingMaterialCostTotal :: Text
    <*> arbitrary -- productionOrderCostingOrderNumber :: Text
    <*> arbitrary -- productionOrderCostingProductionOrderId :: Text
    <*> arbitrary -- productionOrderCostingQuantity :: Integer
    <*> arbitraryReducedMaybe n -- productionOrderCostingSalePrice :: Maybe Text
    <*> arbitrary -- productionOrderCostingStatus :: Text
  
instance Arbitrary ProductionOrderStatusUpdate where
  arbitrary = sized genProductionOrderStatusUpdate

genProductionOrderStatusUpdate :: Int -> Gen ProductionOrderStatusUpdate
genProductionOrderStatusUpdate n =
  ProductionOrderStatusUpdate
    <$> arbitrary -- productionOrderStatusUpdateStatus :: Text
  
instance Arbitrary ProformaInvoice where
  arbitrary = sized genProformaInvoice

genProformaInvoice :: Int -> Gen ProformaInvoice
genProformaInvoice n =
  ProformaInvoice
    <$> arbitraryReducedMaybe n -- proformaInvoiceConvertedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- proformaInvoiceConvertedToInvoiceId :: Maybe Text
    <*> arbitraryReduced n -- proformaInvoiceCurrency :: CurrencyCode
    <*> arbitraryReducedMaybe n -- proformaInvoiceCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceCustomerSnapshot :: Maybe AnyType
    <*> arbitraryReduced n -- proformaInvoiceIssueDate :: Date
    <*> arbitraryReduced n -- proformaInvoiceLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- proformaInvoiceNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceOrderNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoicePaymentDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- proformaInvoiceQuotationId :: Maybe Text
    <*> arbitraryReduced n -- proformaInvoiceStatus :: ProformaInvoiceStatus
    <*> arbitrary -- proformaInvoiceSubtotal :: Text
    <*> arbitrary -- proformaInvoiceTotalAmount :: Text
    <*> arbitrary -- proformaInvoiceTotalTax :: Text
  
instance Arbitrary ProformaInvoiceCreate where
  arbitrary = sized genProformaInvoiceCreate

genProformaInvoiceCreate :: Int -> Gen ProformaInvoiceCreate
genProformaInvoiceCreate n =
  ProformaInvoiceCreate
    <$> arbitraryReducedMaybe n -- proformaInvoiceCreateConvertedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- proformaInvoiceCreateConvertedToInvoiceId :: Maybe Text
    <*> arbitraryReduced n -- proformaInvoiceCreateCurrency :: CurrencyCode
    <*> arbitraryReducedMaybe n -- proformaInvoiceCreateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceCreateCustomerSnapshot :: Maybe AnyType
    <*> arbitraryReduced n -- proformaInvoiceCreateIssueDate :: Date
    <*> arbitraryReduced n -- proformaInvoiceCreateLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- proformaInvoiceCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceCreateOrderNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceCreatePaymentDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- proformaInvoiceCreateQuotationId :: Maybe Text
    <*> arbitraryReduced n -- proformaInvoiceCreateStatus :: ProformaInvoiceStatus
    <*> arbitrary -- proformaInvoiceCreateSubtotal :: Text
    <*> arbitrary -- proformaInvoiceCreateTotalAmount :: Text
    <*> arbitrary -- proformaInvoiceCreateTotalTax :: Text
  
instance Arbitrary ProformaInvoiceUpdate where
  arbitrary = sized genProformaInvoiceUpdate

genProformaInvoiceUpdate :: Int -> Gen ProformaInvoiceUpdate
genProformaInvoiceUpdate n =
  ProformaInvoiceUpdate
    <$> arbitraryReducedMaybe n -- proformaInvoiceUpdateConvertedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateConvertedToInvoiceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateCurrency :: Maybe CurrencyCode
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateCustomerSnapshot :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateIssueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateOrderNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdatePaymentDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateQuotationId :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateStatus :: Maybe ProformaInvoiceStatus
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateSubtotal :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateTotalAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- proformaInvoiceUpdateTotalTax :: Maybe Text
  
instance Arbitrary ProposedAssignment where
  arbitrary = sized genProposedAssignment

genProposedAssignment :: Int -> Gen ProposedAssignment
genProposedAssignment n =
  ProposedAssignment
    <$> arbitrary -- proposedAssignmentAmountPaid :: Text
    <*> arbitrary -- proposedAssignmentConfidence :: Double
    <*> arbitraryReducedMaybe n -- proposedAssignmentCustomerId :: Maybe Text
    <*> arbitrary -- proposedAssignmentInvoiceId :: Text
    <*> arbitrary -- proposedAssignmentInvoiceNumber :: Text
    <*> arbitrary -- proposedAssignmentOpenAmount :: Text
    <*> arbitrary -- proposedAssignmentPaymentDate :: Text
    <*> arbitrary -- proposedAssignmentPaymentId :: Text
    <*> arbitrary -- proposedAssignmentReason :: Text
    <*> arbitraryReducedMaybe n -- proposedAssignmentReference :: Maybe Text
  
instance Arbitrary ProviderInfo where
  arbitrary = sized genProviderInfo

genProviderInfo :: Int -> Gen ProviderInfo
genProviderInfo n =
  ProviderInfo
    <$> arbitrary -- providerInfoDisplayName :: Text
    <*> arbitrary -- providerInfoName :: Text
    <*> arbitrary -- providerInfoRequiresApiKey :: Bool
    <*> arbitrary -- providerInfoServices :: [Text]
    <*> arbitrary -- providerInfoSupportsLabelCreation :: Bool
    <*> arbitrary -- providerInfoSupportsRateEstimation :: Bool
    <*> arbitrary -- providerInfoSupportsTracking :: Bool
  
instance Arbitrary PublicDeliveryAppointmentRequest where
  arbitrary = sized genPublicDeliveryAppointmentRequest

genPublicDeliveryAppointmentRequest :: Int -> Gen PublicDeliveryAppointmentRequest
genPublicDeliveryAppointmentRequest n =
  PublicDeliveryAppointmentRequest
    <$> arbitrary -- publicDeliveryAppointmentRequestEmail :: Text
    <*> arbitraryReducedMaybe n -- publicDeliveryAppointmentRequestNotes :: Maybe Text
    <*> arbitraryReduced n -- publicDeliveryAppointmentRequestRequestedDate :: Date
    <*> arbitrary -- publicDeliveryAppointmentRequestSupplierName :: Text
    <*> arbitraryReducedMaybe n -- publicDeliveryAppointmentRequestTimeSlot :: Maybe Text
    <*> arbitrary -- publicDeliveryAppointmentRequestWarehouseCode :: Text
  
instance Arbitrary PublicDeliveryAppointmentResponse where
  arbitrary = sized genPublicDeliveryAppointmentResponse

genPublicDeliveryAppointmentResponse :: Int -> Gen PublicDeliveryAppointmentResponse
genPublicDeliveryAppointmentResponse n =
  PublicDeliveryAppointmentResponse
    <$> arbitrary -- publicDeliveryAppointmentResponseAppointmentId :: Text
    <*> arbitrary -- publicDeliveryAppointmentResponseConfirmationHint :: Text
    <*> arbitrary -- publicDeliveryAppointmentResponseMessage :: Text
    <*> arbitrary -- publicDeliveryAppointmentResponseStatus :: Text
  
instance Arbitrary PublicDeliveryAppointmentStatusResponse where
  arbitrary = sized genPublicDeliveryAppointmentStatusResponse

genPublicDeliveryAppointmentStatusResponse :: Int -> Gen PublicDeliveryAppointmentStatusResponse
genPublicDeliveryAppointmentStatusResponse n =
  PublicDeliveryAppointmentStatusResponse
    <$> arbitrary -- publicDeliveryAppointmentStatusResponseAppointmentId :: Text
    <*> arbitraryReduced n -- publicDeliveryAppointmentStatusResponseRequestedDate :: Date
    <*> arbitrary -- publicDeliveryAppointmentStatusResponseStatus :: Text
    <*> arbitraryReducedMaybe n -- publicDeliveryAppointmentStatusResponseTimeSlot :: Maybe Text
    <*> arbitrary -- publicDeliveryAppointmentStatusResponseWarehouseName :: Text
  
instance Arbitrary PublicPosting where
  arbitrary = sized genPublicPosting

genPublicPosting :: Int -> Gen PublicPosting
genPublicPosting n =
  PublicPosting
    <$> arbitraryReducedMaybe n -- publicPostingCurrency :: Maybe Text
    <*> arbitrary -- publicPostingDescription :: Text
    <*> arbitraryReducedMaybe n -- publicPostingEmploymentType :: Maybe Text
    <*> arbitrary -- publicPostingId :: Text
    <*> arbitraryReducedMaybe n -- publicPostingLocation :: Maybe Text
    <*> arbitrary -- publicPostingRemote :: Bool
    <*> arbitrary -- publicPostingRequiredSkills :: [Text]
    <*> arbitraryReducedMaybe n -- publicPostingRequirements :: Maybe Text
    <*> arbitraryReducedMaybe n -- publicPostingSalaryMax :: Maybe Int
    <*> arbitraryReducedMaybe n -- publicPostingSalaryMin :: Maybe Int
    <*> arbitrary -- publicPostingTitle :: Text
  
instance Arbitrary PublicReturnItem where
  arbitrary = sized genPublicReturnItem

genPublicReturnItem :: Int -> Gen PublicReturnItem
genPublicReturnItem n =
  PublicReturnItem
    <$> arbitraryReducedMaybe n -- publicReturnItemName :: Maybe Text
    <*> arbitrary -- publicReturnItemProductId :: Text
    <*> arbitrary -- publicReturnItemQuantity :: Integer
    <*> arbitraryReducedMaybe n -- publicReturnItemReason :: Maybe Text
  
instance Arbitrary PublicReturnRequest where
  arbitrary = sized genPublicReturnRequest

genPublicReturnRequest :: Int -> Gen PublicReturnRequest
genPublicReturnRequest n =
  PublicReturnRequest
    <$> arbitrary -- publicReturnRequestEmail :: Text
    <*> arbitraryReduced n -- publicReturnRequestItems :: [PublicReturnItem]
    <*> arbitraryReducedMaybe n -- publicReturnRequestNotes :: Maybe Text
    <*> arbitrary -- publicReturnRequestOrderNumber :: Text
  
instance Arbitrary PublicReturnResponse where
  arbitrary = sized genPublicReturnResponse

genPublicReturnResponse :: Int -> Gen PublicReturnResponse
genPublicReturnResponse n =
  PublicReturnResponse
    <$> arbitraryReduced n -- publicReturnResponseCreatedAt :: DateTime
    <*> arbitraryReduced n -- publicReturnResponseItems :: AnyType
    <*> arbitraryReducedMaybe n -- publicReturnResponseNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- publicReturnResponseOrderNumber :: Maybe Text
    <*> arbitrary -- publicReturnResponseReturnNumber :: Text
    <*> arbitrary -- publicReturnResponseReturnOrderId :: Text
    <*> arbitrary -- publicReturnResponseStatus :: Text
    <*> arbitraryReducedMaybe n -- publicReturnResponseUpdatedAt :: Maybe DateTime
  
instance Arbitrary PublicReturnStatusResponse where
  arbitrary = sized genPublicReturnStatusResponse

genPublicReturnStatusResponse :: Int -> Gen PublicReturnStatusResponse
genPublicReturnStatusResponse n =
  PublicReturnStatusResponse
    <$> arbitraryReduced n -- publicReturnStatusResponseCreatedAt :: DateTime
    <*> arbitraryReduced n -- publicReturnStatusResponseItems :: AnyType
    <*> arbitraryReducedMaybe n -- publicReturnStatusResponseNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- publicReturnStatusResponseOrderNumber :: Maybe Text
    <*> arbitrary -- publicReturnStatusResponseReturnNumber :: Text
    <*> arbitrary -- publicReturnStatusResponseReturnOrderId :: Text
    <*> arbitrary -- publicReturnStatusResponseStatus :: Text
    <*> arbitraryReducedMaybe n -- publicReturnStatusResponseUpdatedAt :: Maybe DateTime
  
instance Arbitrary PurchaseOrder where
  arbitrary = sized genPurchaseOrder

genPurchaseOrder :: Int -> Gen PurchaseOrder
genPurchaseOrder n =
  PurchaseOrder
    <$> arbitraryReducedMaybe n -- purchaseOrderCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderDeliveryAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- purchaseOrderExpectedDeliveryDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- purchaseOrderLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- purchaseOrderNotes :: Maybe Text
    <*> arbitraryReduced n -- purchaseOrderOrderDate :: Date
    <*> arbitrary -- purchaseOrderPoNumber :: Text
    <*> arbitraryReduced n -- purchaseOrderStatus :: PurchaseOrderStatus
    <*> arbitraryReducedMaybe n -- purchaseOrderSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderSupplierName :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderTotalGrossAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderTotalNetAmount :: Maybe Text
  
instance Arbitrary PurchaseOrderCreate where
  arbitrary = sized genPurchaseOrderCreate

genPurchaseOrderCreate :: Int -> Gen PurchaseOrderCreate
genPurchaseOrderCreate n =
  PurchaseOrderCreate
    <$> arbitraryReducedMaybe n -- purchaseOrderCreateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderCreateDeliveryAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- purchaseOrderCreateExpectedDeliveryDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- purchaseOrderCreateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- purchaseOrderCreateNotes :: Maybe Text
    <*> arbitraryReduced n -- purchaseOrderCreateOrderDate :: Date
    <*> arbitrary -- purchaseOrderCreatePoNumber :: Text
    <*> arbitraryReduced n -- purchaseOrderCreateStatus :: PurchaseOrderStatus
    <*> arbitraryReducedMaybe n -- purchaseOrderCreateSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderCreateSupplierName :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderCreateTotalGrossAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderCreateTotalNetAmount :: Maybe Text
  
instance Arbitrary PurchaseOrderStatusUpdate where
  arbitrary = sized genPurchaseOrderStatusUpdate

genPurchaseOrderStatusUpdate :: Int -> Gen PurchaseOrderStatusUpdate
genPurchaseOrderStatusUpdate n =
  PurchaseOrderStatusUpdate
    <$> arbitrary -- purchaseOrderStatusUpdateStatus :: Text
  
instance Arbitrary PurchaseOrderUpdate where
  arbitrary = sized genPurchaseOrderUpdate

genPurchaseOrderUpdate :: Int -> Gen PurchaseOrderUpdate
genPurchaseOrderUpdate n =
  PurchaseOrderUpdate
    <$> arbitraryReducedMaybe n -- purchaseOrderUpdateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateDeliveryAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateExpectedDeliveryDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateOrderDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdatePoNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateStatus :: Maybe PurchaseOrderStatus
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateSupplierName :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateTotalGrossAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- purchaseOrderUpdateTotalNetAmount :: Maybe Text
  
instance Arbitrary QRCodeResponse where
  arbitrary = sized genQRCodeResponse

genQRCodeResponse :: Int -> Gen QRCodeResponse
genQRCodeResponse n =
  QRCodeResponse
    <$> arbitrary -- qRCodeResponseContentType :: Text
    <*> arbitrary -- qRCodeResponseQrCodeBase64 :: Text
  
instance Arbitrary QuartileBand where
  arbitrary = sized genQuartileBand

genQuartileBand :: Int -> Gen QuartileBand
genQuartileBand n =
  QuartileBand
    <$> arbitrary -- quartileBandFemaleSharePct :: Double
    <*> arbitrary -- quartileBandHourlyMedian :: Text
    <*> arbitrary -- quartileBandMaleSharePct :: Double
    <*> arbitrary -- quartileBandQuartile :: Text
  
instance Arbitrary QuizQuestion where
  arbitrary = sized genQuizQuestion

genQuizQuestion :: Int -> Gen QuizQuestion
genQuizQuestion n =
  QuizQuestion
    <$> arbitrary -- quizQuestionId :: Text
    <*> arbitrary -- quizQuestionOptions :: [Text]
    <*> arbitrary -- quizQuestionOptionsEn :: [Text]
    <*> arbitrary -- quizQuestionQuestion :: Text
    <*> arbitrary -- quizQuestionQuestionEn :: Text
  
instance Arbitrary QuotaOverride where
  arbitrary = sized genQuotaOverride

genQuotaOverride :: Int -> Gen QuotaOverride
genQuotaOverride n =
  QuotaOverride
    <$> arbitraryReducedMaybe n -- quotaOverrideFeatures :: Maybe QuotaOverrideFeatures
    <*> arbitraryReducedMaybe n -- quotaOverrideMaxConnectors :: Maybe Int
    <*> arbitraryReducedMaybe n -- quotaOverrideMaxInvoicesPerMonth :: Maybe Integer
    <*> arbitraryReducedMaybe n -- quotaOverrideMaxUsers :: Maybe Int
    <*> arbitraryReducedMaybe n -- quotaOverrideMetered :: Maybe (Map.Map String Integer)
    <*> arbitraryReducedMaybe n -- quotaOverridePlan :: Maybe Text
  
instance Arbitrary QuotaOverrideFeatures where
  arbitrary = sized genQuotaOverrideFeatures

genQuotaOverrideFeatures :: Int -> Gen QuotaOverrideFeatures
genQuotaOverrideFeatures n =
  QuotaOverrideFeatures
    <$> arbitraryReducedMaybe n -- quotaOverrideFeaturesErp :: Maybe Bool
    <*> arbitraryReducedMaybe n -- quotaOverrideFeaturesFancyReports :: Maybe Bool
    <*> arbitraryReducedMaybe n -- quotaOverrideFeaturesTaxAutomations :: Maybe Bool
  
instance Arbitrary QuotaOverview where
  arbitrary = sized genQuotaOverview

genQuotaOverview :: Int -> Gen QuotaOverview
genQuotaOverview n =
  QuotaOverview
    <$> arbitraryReduced n -- quotaOverviewFeatures :: PlanFeatures
    <*> arbitrary -- quotaOverviewIsTrialing :: Bool
    <*> arbitraryReduced n -- quotaOverviewLimits :: PlanLimits
    <*> arbitraryReduced n -- quotaOverviewMetered :: [MeteredUsage]
    <*> arbitrary -- quotaOverviewPlan :: Text
    <*> arbitrary -- quotaOverviewPlanName :: Text
    <*> arbitraryReducedMaybe n -- quotaOverviewTrialEndsAt :: Maybe DateTime
    <*> arbitraryReduced n -- quotaOverviewUsage :: UsageSnapshot
  
instance Arbitrary Quotation where
  arbitrary = sized genQuotation

genQuotation :: Int -> Gen Quotation
genQuotation n =
  Quotation
    <$> arbitraryReducedMaybe n -- quotationAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- quotationContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationContactName :: Maybe Text
    <*> arbitrary -- quotationCurrency :: Text
    <*> arbitraryReducedMaybe n -- quotationExpirationDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- quotationFiles :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- quotationIntroduction :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- quotationPrecedingSalesVoucherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationPrecedingSalesVoucherType :: Maybe PrecedingSalesVoucherType
    <*> arbitraryReducedMaybe n -- quotationQuotationNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationRemark :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationSubtotal :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationTaxCondition :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationTotalAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationTotalTax :: Maybe Text
    <*> arbitraryReduced n -- quotationVoucherDate :: Date
    <*> arbitraryReduced n -- quotationVoucherStatus :: VoucherStatus
  
instance Arbitrary QuotationCreate where
  arbitrary = sized genQuotationCreate

genQuotationCreate :: Int -> Gen QuotationCreate
genQuotationCreate n =
  QuotationCreate
    <$> arbitraryReducedMaybe n -- quotationCreateAddress :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- quotationCreateContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationCreateContactName :: Maybe Text
    <*> arbitrary -- quotationCreateCurrency :: Text
    <*> arbitraryReducedMaybe n -- quotationCreateExpirationDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- quotationCreateFiles :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- quotationCreateIntroduction :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationCreateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- quotationCreatePrecedingSalesVoucherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationCreatePrecedingSalesVoucherType :: Maybe PrecedingSalesVoucherType
    <*> arbitraryReducedMaybe n -- quotationCreateQuotationNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationCreateRemark :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationCreateTaxCondition :: Maybe Text
    <*> arbitraryReducedMaybe n -- quotationCreateTitle :: Maybe Text
    <*> arbitraryReduced n -- quotationCreateVoucherDate :: Date
    <*> arbitraryReduced n -- quotationCreateVoucherStatus :: VoucherStatus
  
instance Arbitrary RateRequest where
  arbitrary = sized genRateRequest

genRateRequest :: Int -> Gen RateRequest
genRateRequest n =
  RateRequest
    <$> arbitraryReducedMaybe n -- rateRequestCustomer :: Maybe CustomerInfo
    <*> arbitraryReduced n -- rateRequestPackages :: [Package]
    <*> arbitraryReduced n -- rateRequestRecipient :: Address
    <*> arbitraryReduced n -- rateRequestSender :: Address
  
instance Arbitrary RateResponse where
  arbitrary = sized genRateResponse

genRateResponse :: Int -> Gen RateResponse
genRateResponse n =
  RateResponse
    <$> arbitraryReduced n -- rateResponseRates :: [ShippingRate]
  
instance Arbitrary RecurringTemplate where
  arbitrary = sized genRecurringTemplate

genRecurringTemplate :: Int -> Gen RecurringTemplate
genRecurringTemplate n =
  RecurringTemplate
    <$> arbitrary -- recurringTemplateCreatedAt :: Text
    <*> arbitraryReducedMaybe n -- recurringTemplateDeletedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- recurringTemplateEndDate :: Maybe Date
    <*> arbitrary -- recurringTemplateExecutionInterval :: Text
    <*> arbitrary -- recurringTemplateExecutionStatus :: Text
    <*> arbitrary -- recurringTemplateFinalize :: Bool
    <*> arbitraryReducedMaybe n -- recurringTemplateLastExecutedAt :: Maybe DateTime
    <*> arbitrary -- recurringTemplateName :: Text
    <*> arbitraryReducedMaybe n -- recurringTemplateNextExecutionAt :: Maybe DateTime
    <*> arbitraryReduced n -- recurringTemplateStartDate :: Date
    <*> arbitrary -- recurringTemplateTemplateId :: Text
    <*> arbitrary -- recurringTemplateTemplateType :: Text
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdatedAt :: Maybe Text
    <*> arbitraryReduced n -- recurringTemplateVoucherData :: AnyType
  
instance Arbitrary RecurringTemplateCreate where
  arbitrary = sized genRecurringTemplateCreate

genRecurringTemplateCreate :: Int -> Gen RecurringTemplateCreate
genRecurringTemplateCreate n =
  RecurringTemplateCreate
    <$> arbitraryReducedMaybe n -- recurringTemplateCreateEndDate :: Maybe Date
    <*> arbitrary -- recurringTemplateCreateExecutionInterval :: Text
    <*> arbitraryReduced n -- recurringTemplateCreateExecutionStatus :: ExecutionStatus
    <*> arbitraryReducedMaybe n -- recurringTemplateCreateFinalize :: Maybe Bool
    <*> arbitraryReducedMaybe n -- recurringTemplateCreateLastExecutedAt :: Maybe DateTime
    <*> arbitrary -- recurringTemplateCreateName :: Text
    <*> arbitraryReducedMaybe n -- recurringTemplateCreateNextExecutionAt :: Maybe DateTime
    <*> arbitraryReduced n -- recurringTemplateCreateStartDate :: Date
    <*> arbitraryReduced n -- recurringTemplateCreateTemplateType :: RecurringTemplateType
    <*> arbitraryReducedMaybe n -- recurringTemplateCreateVoucherData :: Maybe AnyType
  
instance Arbitrary RecurringTemplateUpdate where
  arbitrary = sized genRecurringTemplateUpdate

genRecurringTemplateUpdate :: Int -> Gen RecurringTemplateUpdate
genRecurringTemplateUpdate n =
  RecurringTemplateUpdate
    <$> arbitraryReducedMaybe n -- recurringTemplateUpdateEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdateExecutionInterval :: Maybe Text
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdateExecutionStatus :: Maybe ExecutionStatus
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdateFinalize :: Maybe Bool
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdateLastExecutedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdateNextExecutionAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdateStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdateTemplateType :: Maybe RecurringTemplateType
    <*> arbitraryReducedMaybe n -- recurringTemplateUpdateVoucherData :: Maybe AnyType
  
instance Arbitrary RegisterRequest where
  arbitrary = sized genRegisterRequest

genRegisterRequest :: Int -> Gen RegisterRequest
genRegisterRequest n =
  RegisterRequest
    <$> arbitrary -- registerRequestCompanyName :: Text
    <*> arbitrary -- registerRequestEmail :: Text
    <*> arbitrary -- registerRequestFirstName :: Text
    <*> arbitrary -- registerRequestLastName :: Text
    <*> arbitrary -- registerRequestPassword :: Text
    <*> arbitrary -- registerRequestPrivacyAccepted :: Bool
  
instance Arbitrary RemoveUserRequest where
  arbitrary = sized genRemoveUserRequest

genRemoveUserRequest :: Int -> Gen RemoveUserRequest
genRemoveUserRequest n =
  RemoveUserRequest
    <$> arbitrary -- removeUserRequestEmail :: Text
  
instance Arbitrary ReorderProposalLine where
  arbitrary = sized genReorderProposalLine

genReorderProposalLine :: Int -> Gen ReorderProposalLine
genReorderProposalLine n =
  ReorderProposalLine
    <$> arbitrary -- reorderProposalLineCurrentStock :: Integer
    <*> arbitraryReducedMaybe n -- reorderProposalLineMaxStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- reorderProposalLineMinStock :: Maybe Integer
    <*> arbitrary -- reorderProposalLineProductId :: Text
    <*> arbitrary -- reorderProposalLineProductName :: Text
    <*> arbitraryReducedMaybe n -- reorderProposalLineReorderQuantity :: Maybe Integer
    <*> arbitrary -- reorderProposalLineSku :: Text
    <*> arbitrary -- reorderProposalLineSuggestedQuantity :: Integer
  
instance Arbitrary ReorderProposalResponse where
  arbitrary = sized genReorderProposalResponse

genReorderProposalResponse :: Int -> Gen ReorderProposalResponse
genReorderProposalResponse n =
  ReorderProposalResponse
    <$> arbitraryReduced n -- reorderProposalResponseGeneratedAt :: DateTime
    <*> arbitraryReduced n -- reorderProposalResponseLines :: [ReorderProposalLine]
    <*> arbitrary -- reorderProposalResponseTotalSuggestedQuantity :: Integer
  
instance Arbitrary ReplenishmentResponse where
  arbitrary = sized genReplenishmentResponse

genReplenishmentResponse :: Int -> Gen ReplenishmentResponse
genReplenishmentResponse n =
  ReplenishmentResponse
    <$> arbitraryReduced n -- replenishmentResponseGeneratedAt :: DateTime
    <*> arbitraryReduced n -- replenishmentResponseLines :: [ReplenishmentSuggestionLine]
    <*> arbitrary -- replenishmentResponseTargetWarehouseId :: Text
    <*> arbitrary -- replenishmentResponseTotalSuggestedQuantity :: Integer
  
instance Arbitrary ReplenishmentSuggestionLine where
  arbitrary = sized genReplenishmentSuggestionLine

genReplenishmentSuggestionLine :: Int -> Gen ReplenishmentSuggestionLine
genReplenishmentSuggestionLine n =
  ReplenishmentSuggestionLine
    <$> arbitrary -- replenishmentSuggestionLineCurrentStock :: Integer
    <*> arbitraryReducedMaybe n -- replenishmentSuggestionLineMaxStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- replenishmentSuggestionLineMinStock :: Maybe Integer
    <*> arbitrary -- replenishmentSuggestionLineProductId :: Text
    <*> arbitrary -- replenishmentSuggestionLineProductName :: Text
    <*> arbitrary -- replenishmentSuggestionLineSku :: Text
    <*> arbitrary -- replenishmentSuggestionLineSourceAvailable :: Integer
    <*> arbitrary -- replenishmentSuggestionLineSourceWarehouseId :: Text
    <*> arbitrary -- replenishmentSuggestionLineSuggestedQuantity :: Integer
    <*> arbitrary -- replenishmentSuggestionLineTargetWarehouseId :: Text
  
instance Arbitrary ResetPasswordRequest where
  arbitrary = sized genResetPasswordRequest

genResetPasswordRequest :: Int -> Gen ResetPasswordRequest
genResetPasswordRequest n =
  ResetPasswordRequest
    <$> arbitrary -- resetPasswordRequestNewPassword :: Text
    <*> arbitrary -- resetPasswordRequestToken :: Text
  
instance Arbitrary ResolvedPriceResponse where
  arbitrary = sized genResolvedPriceResponse

genResolvedPriceResponse :: Int -> Gen ResolvedPriceResponse
genResolvedPriceResponse n =
  ResolvedPriceResponse
    <$> arbitrary -- resolvedPriceResponseIsListPrice :: Bool
    <*> arbitraryReducedMaybe n -- resolvedPriceResponsePriceTierId :: Maybe Text
    <*> arbitrary -- resolvedPriceResponseProductId :: Text
    <*> arbitrary -- resolvedPriceResponseQuantity :: Integer
    <*> arbitrary -- resolvedPriceResponseUnitPrice :: Text
  
instance Arbitrary ReturnLogisticsQueueItem where
  arbitrary = sized genReturnLogisticsQueueItem

genReturnLogisticsQueueItem :: Int -> Gen ReturnLogisticsQueueItem
genReturnLogisticsQueueItem n =
  ReturnLogisticsQueueItem
    <$> arbitrary -- returnLogisticsQueueItemAgeDays :: Integer
    <*> arbitraryReduced n -- returnLogisticsQueueItemCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- returnLogisticsQueueItemCustomerName :: Maybe Text
    <*> arbitraryReduced n -- returnLogisticsQueueItemLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- returnLogisticsQueueItemOrderNumber :: Maybe Text
    <*> arbitrary -- returnLogisticsQueueItemReturnNumber :: Text
    <*> arbitrary -- returnLogisticsQueueItemReturnOrderId :: Text
    <*> arbitrary -- returnLogisticsQueueItemStatus :: Text
    <*> arbitraryReducedMaybe n -- returnLogisticsQueueItemWarehouseId :: Maybe Text
  
instance Arbitrary ReturnLogisticsSummary where
  arbitrary = sized genReturnLogisticsSummary

genReturnLogisticsSummary :: Int -> Gen ReturnLogisticsSummary
genReturnLogisticsSummary n =
  ReturnLogisticsSummary
    <$> arbitraryReduced n -- returnLogisticsSummaryByStatus :: AnyType
    <*> arbitraryReduced n -- returnLogisticsSummaryByWarehouse :: [ReturnWarehouseSummary]
    <*> arbitrary -- returnLogisticsSummaryItemsRestocked :: Integer
    <*> arbitrary -- returnLogisticsSummaryItemsScrapped :: Integer
    <*> arbitrary -- returnLogisticsSummaryTotalItems :: Integer
    <*> arbitrary -- returnLogisticsSummaryTotalReturns :: Integer
  
instance Arbitrary ReturnOrder where
  arbitrary = sized genReturnOrder

genReturnOrder :: Int -> Gen ReturnOrder
genReturnOrder n =
  ReturnOrder
    <$> arbitraryReducedMaybe n -- returnOrderCustomerContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- returnOrderCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- returnOrderLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- returnOrderNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- returnOrderOrderId :: Maybe Text
    <*> arbitraryReducedMaybe n -- returnOrderOrderNumber :: Maybe Text
    <*> arbitrary -- returnOrderReturnNumber :: Text
    <*> arbitraryReducedMaybe n -- returnOrderReturnReason :: Maybe Text
    <*> arbitraryReduced n -- returnOrderStatus :: ReturnOrderStatus
    <*> arbitraryReducedMaybe n -- returnOrderWarehouseId :: Maybe Text
  
instance Arbitrary ReturnOrderStatusUpdate where
  arbitrary = sized genReturnOrderStatusUpdate

genReturnOrderStatusUpdate :: Int -> Gen ReturnOrderStatusUpdate
genReturnOrderStatusUpdate n =
  ReturnOrderStatusUpdate
    <$> arbitrary -- returnOrderStatusUpdateStatus :: Text
  
instance Arbitrary ReturnWarehouseSummary where
  arbitrary = sized genReturnWarehouseSummary

genReturnWarehouseSummary :: Int -> Gen ReturnWarehouseSummary
genReturnWarehouseSummary n =
  ReturnWarehouseSummary
    <$> arbitrary -- returnWarehouseSummaryItemsRestocked :: Integer
    <*> arbitrary -- returnWarehouseSummaryItemsScrapped :: Integer
    <*> arbitrary -- returnWarehouseSummaryReturns :: Integer
    <*> arbitraryReducedMaybe n -- returnWarehouseSummaryWarehouseId :: Maybe Text
  
instance Arbitrary RevenueItem where
  arbitrary = sized genRevenueItem

genRevenueItem :: Int -> Gen RevenueItem
genRevenueItem n =
  RevenueItem
    <$> arbitrary -- revenueItemAmount :: Text
    <*> arbitrary -- revenueItemCategory :: Text
    <*> arbitrary -- revenueItemPercentage :: Double
  
instance Arbitrary Rfq where
  arbitrary = sized genRfq

genRfq :: Int -> Gen Rfq
genRfq n =
  Rfq
    <$> arbitraryReducedMaybe n -- rfqCurrency :: Maybe Text
    <*> arbitraryReduced n -- rfqLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- rfqNotes :: Maybe Text
    <*> arbitraryReduced n -- rfqRequestedDate :: Date
    <*> arbitraryReducedMaybe n -- rfqResponseDate :: Maybe Date
    <*> arbitrary -- rfqRfqNumber :: Text
    <*> arbitraryReduced n -- rfqStatus :: RfqStatus
    <*> arbitraryReducedMaybe n -- rfqSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- rfqSupplierName :: Maybe Text
  
instance Arbitrary RfqCreate where
  arbitrary = sized genRfqCreate

genRfqCreate :: Int -> Gen RfqCreate
genRfqCreate n =
  RfqCreate
    <$> arbitraryReducedMaybe n -- rfqCreateCurrency :: Maybe Text
    <*> arbitraryReduced n -- rfqCreateLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- rfqCreateNotes :: Maybe Text
    <*> arbitraryReduced n -- rfqCreateRequestedDate :: Date
    <*> arbitraryReducedMaybe n -- rfqCreateResponseDate :: Maybe Date
    <*> arbitrary -- rfqCreateRfqNumber :: Text
    <*> arbitraryReduced n -- rfqCreateStatus :: RfqStatus
    <*> arbitraryReducedMaybe n -- rfqCreateSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- rfqCreateSupplierName :: Maybe Text
  
instance Arbitrary RfqStatusUpdate where
  arbitrary = sized genRfqStatusUpdate

genRfqStatusUpdate :: Int -> Gen RfqStatusUpdate
genRfqStatusUpdate n =
  RfqStatusUpdate
    <$> arbitrary -- rfqStatusUpdateStatus :: Text
  
instance Arbitrary RfqUpdate where
  arbitrary = sized genRfqUpdate

genRfqUpdate :: Int -> Gen RfqUpdate
genRfqUpdate n =
  RfqUpdate
    <$> arbitraryReducedMaybe n -- rfqUpdateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- rfqUpdateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- rfqUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- rfqUpdateRequestedDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- rfqUpdateResponseDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- rfqUpdateRfqNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- rfqUpdateStatus :: Maybe RfqStatus
    <*> arbitraryReducedMaybe n -- rfqUpdateSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- rfqUpdateSupplierName :: Maybe Text
  
instance Arbitrary SalesVolumeItem where
  arbitrary = sized genSalesVolumeItem

genSalesVolumeItem :: Int -> Gen SalesVolumeItem
genSalesVolumeItem n =
  SalesVolumeItem
    <$> arbitrary -- salesVolumeItemContactId :: Text
    <*> arbitrary -- salesVolumeItemContactType :: Text
    <*> arbitraryReducedMaybe n -- salesVolumeItemLastPurchaseDate :: Maybe Text
    <*> arbitrary -- salesVolumeItemName :: Text
    <*> arbitrary -- salesVolumeItemTotalInvoices :: Int
    <*> arbitrary -- salesVolumeItemTotalRevenue :: Text
  
instance Arbitrary SalesVolumeReport where
  arbitrary = sized genSalesVolumeReport

genSalesVolumeReport :: Int -> Gen SalesVolumeReport
genSalesVolumeReport n =
  SalesVolumeReport
    <$> arbitrary -- salesVolumeReportGrandTotal :: Text
    <*> arbitraryReduced n -- salesVolumeReportItems :: [SalesVolumeItem]
    <*> arbitrary -- salesVolumeReportTotalCount :: Integer
  
instance Arbitrary ScopeTotal where
  arbitrary = sized genScopeTotal

genScopeTotal :: Int -> Gen ScopeTotal
genScopeTotal n =
  ScopeTotal
    <$> arbitrary -- scopeTotalScope :: Text
    <*> arbitrary -- scopeTotalTco2e :: Text
  
instance Arbitrary Section where
  arbitrary = sized genSection

genSection :: Int -> Gen Section
genSection n =
  Section
    <$> arbitrary -- sectionBodyHtml :: Text
    <*> arbitrary -- sectionBodyHtmlEn :: Text
    <*> arbitrary -- sectionId :: Text
    <*> arbitrary -- sectionTitle :: Text
    <*> arbitrary -- sectionTitleEn :: Text
  
instance Arbitrary SendMessageDto where
  arbitrary = sized genSendMessageDto

genSendMessageDto :: Int -> Gen SendMessageDto
genSendMessageDto n =
  SendMessageDto
    <$> arbitrary -- sendMessageDtoBody :: Text
    <*> arbitraryReducedMaybe n -- sendMessageDtoIsInternal :: Maybe Bool
  
instance Arbitrary SepaDirectDebitResponse where
  arbitrary = sized genSepaDirectDebitResponse

genSepaDirectDebitResponse :: Int -> Gen SepaDirectDebitResponse
genSepaDirectDebitResponse n =
  SepaDirectDebitResponse
    <$> arbitrary -- sepaDirectDebitResponseContentType :: Text
    <*> arbitrary -- sepaDirectDebitResponseFilename :: Text
    <*> arbitrary -- sepaDirectDebitResponseXmlContent :: Text
  
instance Arbitrary ServiceAssignment where
  arbitrary = sized genServiceAssignment

genServiceAssignment :: Int -> Gen ServiceAssignment
genServiceAssignment n =
  ServiceAssignment
    <$> arbitraryReducedMaybe n -- serviceAssignmentEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentJobId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentScheduledDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- serviceAssignmentScheduledEnd :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentScheduledStart :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentStatus :: Maybe ServiceAssignmentStatus
  
instance Arbitrary ServiceAssignmentCreate where
  arbitrary = sized genServiceAssignmentCreate

genServiceAssignmentCreate :: Int -> Gen ServiceAssignmentCreate
genServiceAssignmentCreate n =
  ServiceAssignmentCreate
    <$> arbitraryReducedMaybe n -- serviceAssignmentCreateEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentCreateJobId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentCreateScheduledDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- serviceAssignmentCreateScheduledEnd :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentCreateScheduledStart :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentCreateStatus :: Maybe ServiceAssignmentStatus
  
instance Arbitrary ServiceAssignmentUpdate where
  arbitrary = sized genServiceAssignmentUpdate

genServiceAssignmentUpdate :: Int -> Gen ServiceAssignmentUpdate
genServiceAssignmentUpdate n =
  ServiceAssignmentUpdate
    <$> arbitraryReducedMaybe n -- serviceAssignmentUpdateEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentUpdateJobId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentUpdateScheduledDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- serviceAssignmentUpdateScheduledEnd :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentUpdateScheduledStart :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceAssignmentUpdateStatus :: Maybe ServiceAssignmentStatus
  
instance Arbitrary ServiceJob where
  arbitrary = sized genServiceJob

genServiceJob :: Int -> Gen ServiceJob
genServiceJob n =
  ServiceJob
    <$> arbitraryReducedMaybe n -- serviceJobAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCustomerEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCustomerPhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobEstimatedDurationMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- serviceJobLat :: Maybe Double
    <*> arbitraryReducedMaybe n -- serviceJobLng :: Maybe Double
    <*> arbitraryReducedMaybe n -- serviceJobNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobStatus :: Maybe ServiceJobStatus
  
instance Arbitrary ServiceJobCreate where
  arbitrary = sized genServiceJobCreate

genServiceJobCreate :: Int -> Gen ServiceJobCreate
genServiceJobCreate n =
  ServiceJobCreate
    <$> arbitraryReducedMaybe n -- serviceJobCreateAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCreateCustomerEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCreateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCreateCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCreateCustomerPhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCreateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCreateEstimatedDurationMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- serviceJobCreateLat :: Maybe Double
    <*> arbitraryReducedMaybe n -- serviceJobCreateLng :: Maybe Double
    <*> arbitraryReducedMaybe n -- serviceJobCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobCreateStatus :: Maybe ServiceJobStatus
  
instance Arbitrary ServiceJobUpdate where
  arbitrary = sized genServiceJobUpdate

genServiceJobUpdate :: Int -> Gen ServiceJobUpdate
genServiceJobUpdate n =
  ServiceJobUpdate
    <$> arbitraryReducedMaybe n -- serviceJobUpdateAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobUpdateCustomerEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobUpdateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobUpdateCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobUpdateCustomerPhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobUpdateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobUpdateEstimatedDurationMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- serviceJobUpdateLat :: Maybe Double
    <*> arbitraryReducedMaybe n -- serviceJobUpdateLng :: Maybe Double
    <*> arbitraryReducedMaybe n -- serviceJobUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceJobUpdateStatus :: Maybe ServiceJobStatus
  
instance Arbitrary Shareholder where
  arbitrary = sized genShareholder

genShareholder :: Int -> Gen Shareholder
genShareholder n =
  Shareholder
    <$> arbitraryReducedMaybe n -- shareholderAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderBirthDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- shareholderEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderShareNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderShares :: Maybe Text
  
instance Arbitrary ShareholderCreate where
  arbitrary = sized genShareholderCreate

genShareholderCreate :: Int -> Gen ShareholderCreate
genShareholderCreate n =
  ShareholderCreate
    <$> arbitraryReducedMaybe n -- shareholderCreateAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderCreateBirthDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- shareholderCreateEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderCreateFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderCreateLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderCreateShareNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderCreateShares :: Maybe Text
  
instance Arbitrary ShareholderUpdate where
  arbitrary = sized genShareholderUpdate

genShareholderUpdate :: Int -> Gen ShareholderUpdate
genShareholderUpdate n =
  ShareholderUpdate
    <$> arbitraryReducedMaybe n -- shareholderUpdateAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderUpdateBirthDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- shareholderUpdateEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderUpdateFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderUpdateLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderUpdateShareNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- shareholderUpdateShares :: Maybe Text
  
instance Arbitrary Shipment where
  arbitrary = sized genShipment

genShipment :: Int -> Gen Shipment
genShipment n =
  Shipment
    <$> arbitraryReducedMaybe n -- shipmentDeliveredAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- shipmentLabelUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- shipmentLineItemsShipment :: Maybe AnyType
    <*> arbitrary -- shipmentOrderId :: Text
    <*> arbitraryReducedMaybe n -- shipmentRecipientAddress :: Maybe AnyType
    <*> arbitraryReduced n -- shipmentShipmentDate :: Date
    <*> arbitrary -- shipmentShippingCarrier :: Text
    <*> arbitraryReducedMaybe n -- shipmentShippingCost :: Maybe Text
    <*> arbitraryReducedMaybe n -- shipmentShippingMethod :: Maybe Text
    <*> arbitraryReducedMaybe n -- shipmentSignedBy :: Maybe Text
    <*> arbitrary -- shipmentStatus :: Text
    <*> arbitraryReducedMaybe n -- shipmentTrackingEvents :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- shipmentTrackingNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- shipmentTrackingUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- shipmentWeightKg :: Maybe Double
  
instance Arbitrary ShipmentStatusUpdate where
  arbitrary = sized genShipmentStatusUpdate

genShipmentStatusUpdate :: Int -> Gen ShipmentStatusUpdate
genShipmentStatusUpdate n =
  ShipmentStatusUpdate
    <$> arbitraryReducedMaybe n -- shipmentStatusUpdateDeliveredAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- shipmentStatusUpdateSignedBy :: Maybe Text
    <*> arbitrary -- shipmentStatusUpdateStatus :: Text
    <*> arbitraryReducedMaybe n -- shipmentStatusUpdateTrackingNumber :: Maybe Text
  
instance Arbitrary ShippingCredentials where
  arbitrary = sized genShippingCredentials

genShippingCredentials :: Int -> Gen ShippingCredentials
genShippingCredentials n =
  ShippingCredentials
    <$> arbitraryReducedMaybe n -- shippingCredentialsDhl :: Maybe DhlCredentials
    <*> arbitraryReducedMaybe n -- shippingCredentialsUps :: Maybe UpsCredentials
  
instance Arbitrary ShippingRate where
  arbitrary = sized genShippingRate

genShippingRate :: Int -> Gen ShippingRate
genShippingRate n =
  ShippingRate
    <$> arbitraryReducedMaybe n -- shippingRateBreakdown :: Maybe Text
    <*> arbitrary -- shippingRateCarrier :: Text
    <*> arbitraryReducedMaybe n -- shippingRateCrossBorderSurcharge :: Maybe Text
    <*> arbitrary -- shippingRateDestinationCountry :: Text
    <*> arbitraryReducedMaybe n -- shippingRateEstimatedDays :: Maybe Int
    <*> arbitrary -- shippingRateFromApi :: Bool
    <*> arbitraryReducedMaybe n -- shippingRateInsuredValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRateIslandSurcharge :: Maybe Text
    <*> arbitrary -- shippingRateOriginCountry :: Text
    <*> arbitrary -- shippingRateRate :: Text
    <*> arbitrary -- shippingRateService :: Text
    <*> arbitraryReducedMaybe n -- shippingRateVolumeDiscount :: Maybe Text
    <*> arbitrary -- shippingRateWeightKg :: Double
  
instance Arbitrary ShippingRule where
  arbitrary = sized genShippingRule

genShippingRule :: Int -> Gen ShippingRule
genShippingRule n =
  ShippingRule
    <$> arbitraryReducedMaybe n -- shippingRuleCarrier :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRuleCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- shippingRuleDeliveryTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRuleIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- shippingRuleMaxWeightKg :: Maybe Double
    <*> arbitraryReducedMaybe n -- shippingRuleMinWeightKg :: Maybe Double
    <*> arbitrary -- shippingRuleName :: Text
    <*> arbitraryReducedMaybe n -- shippingRuleNotes :: Maybe Text
    <*> arbitrary -- shippingRulePrice :: Text
    <*> arbitraryReducedMaybe n -- shippingRulePriority :: Maybe Int
  
instance Arbitrary ShippingRuleCreate where
  arbitrary = sized genShippingRuleCreate

genShippingRuleCreate :: Int -> Gen ShippingRuleCreate
genShippingRuleCreate n =
  ShippingRuleCreate
    <$> arbitraryReducedMaybe n -- shippingRuleCreateCarrier :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRuleCreateCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- shippingRuleCreateDeliveryTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRuleCreateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- shippingRuleCreateMaxWeightKg :: Maybe Double
    <*> arbitraryReducedMaybe n -- shippingRuleCreateMinWeightKg :: Maybe Double
    <*> arbitrary -- shippingRuleCreateName :: Text
    <*> arbitraryReducedMaybe n -- shippingRuleCreateNotes :: Maybe Text
    <*> arbitrary -- shippingRuleCreatePrice :: Text
    <*> arbitraryReducedMaybe n -- shippingRuleCreatePriority :: Maybe Int
  
instance Arbitrary ShippingRuleUpdate where
  arbitrary = sized genShippingRuleUpdate

genShippingRuleUpdate :: Int -> Gen ShippingRuleUpdate
genShippingRuleUpdate n =
  ShippingRuleUpdate
    <$> arbitraryReducedMaybe n -- shippingRuleUpdateCarrier :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRuleUpdateCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- shippingRuleUpdateDeliveryTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRuleUpdateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- shippingRuleUpdateMaxWeightKg :: Maybe Double
    <*> arbitraryReducedMaybe n -- shippingRuleUpdateMinWeightKg :: Maybe Double
    <*> arbitraryReducedMaybe n -- shippingRuleUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRuleUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRuleUpdatePrice :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingRuleUpdatePriority :: Maybe Int
  
instance Arbitrary ShippingThreshold where
  arbitrary = sized genShippingThreshold

genShippingThreshold :: Int -> Gen ShippingThreshold
genShippingThreshold n =
  ShippingThreshold
    <$> arbitraryReducedMaybe n -- shippingThresholdIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- shippingThresholdMaxSellable :: Maybe Integer
    <*> arbitrary -- shippingThresholdName :: Text
    <*> arbitraryReducedMaybe n -- shippingThresholdNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingThresholdProductId :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingThresholdReserveStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- shippingThresholdWarehouseId :: Maybe Text
  
instance Arbitrary ShippingThresholdCreate where
  arbitrary = sized genShippingThresholdCreate

genShippingThresholdCreate :: Int -> Gen ShippingThresholdCreate
genShippingThresholdCreate n =
  ShippingThresholdCreate
    <$> arbitraryReducedMaybe n -- shippingThresholdCreateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- shippingThresholdCreateMaxSellable :: Maybe Integer
    <*> arbitrary -- shippingThresholdCreateName :: Text
    <*> arbitraryReducedMaybe n -- shippingThresholdCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingThresholdCreateProductId :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingThresholdCreateReserveStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- shippingThresholdCreateWarehouseId :: Maybe Text
  
instance Arbitrary ShippingThresholdUpdate where
  arbitrary = sized genShippingThresholdUpdate

genShippingThresholdUpdate :: Int -> Gen ShippingThresholdUpdate
genShippingThresholdUpdate n =
  ShippingThresholdUpdate
    <$> arbitraryReducedMaybe n -- shippingThresholdUpdateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- shippingThresholdUpdateMaxSellable :: Maybe Integer
    <*> arbitraryReducedMaybe n -- shippingThresholdUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingThresholdUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingThresholdUpdateProductId :: Maybe Text
    <*> arbitraryReducedMaybe n -- shippingThresholdUpdateReserveStock :: Maybe Integer
    <*> arbitraryReducedMaybe n -- shippingThresholdUpdateWarehouseId :: Maybe Text
  
instance Arbitrary SilentPartner where
  arbitrary = sized genSilentPartner

genSilentPartner :: Int -> Gen SilentPartner
genSilentPartner n =
  SilentPartner
    <$> arbitraryReducedMaybe n -- silentPartnerContractDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- silentPartnerEinlage :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerGewinnquotePct :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerGewinnvortrag :: Maybe Text
    <*> arbitraryReduced n -- silentPartnerInstrumentType :: InstrumentType
    <*> arbitraryReducedMaybe n -- silentPartnerKestPflichtig :: Maybe Bool
    <*> arbitraryReducedMaybe n -- silentPartnerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerVerlustVerrechnungskonto :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerVerlustbeteiligung :: Maybe Bool
  
instance Arbitrary SilentPartnerCreate where
  arbitrary = sized genSilentPartnerCreate

genSilentPartnerCreate :: Int -> Gen SilentPartnerCreate
genSilentPartnerCreate n =
  SilentPartnerCreate
    <$> arbitraryReducedMaybe n -- silentPartnerCreateContractDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- silentPartnerCreateEinlage :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerCreateGewinnquotePct :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerCreateGewinnvortrag :: Maybe Text
    <*> arbitraryReduced n -- silentPartnerCreateInstrumentType :: InstrumentType
    <*> arbitraryReducedMaybe n -- silentPartnerCreateKestPflichtig :: Maybe Bool
    <*> arbitraryReducedMaybe n -- silentPartnerCreateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerCreateVerlustVerrechnungskonto :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerCreateVerlustbeteiligung :: Maybe Bool
  
instance Arbitrary SilentPartnerUpdate where
  arbitrary = sized genSilentPartnerUpdate

genSilentPartnerUpdate :: Int -> Gen SilentPartnerUpdate
genSilentPartnerUpdate n =
  SilentPartnerUpdate
    <$> arbitraryReducedMaybe n -- silentPartnerUpdateContractDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- silentPartnerUpdateEinlage :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerUpdateGewinnquotePct :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerUpdateGewinnvortrag :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerUpdateInstrumentType :: Maybe InstrumentType
    <*> arbitraryReducedMaybe n -- silentPartnerUpdateKestPflichtig :: Maybe Bool
    <*> arbitraryReducedMaybe n -- silentPartnerUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerUpdateVerlustVerrechnungskonto :: Maybe Text
    <*> arbitraryReducedMaybe n -- silentPartnerUpdateVerlustbeteiligung :: Maybe Bool
  
instance Arbitrary SmtpConfig where
  arbitrary = sized genSmtpConfig

genSmtpConfig :: Int -> Gen SmtpConfig
genSmtpConfig n =
  SmtpConfig
    <$> arbitraryReduced n -- smtpConfigEncryption :: SmtpEncryption
    <*> arbitrary -- smtpConfigFromAddress :: Text
    <*> arbitraryReducedMaybe n -- smtpConfigFromName :: Maybe Text
    <*> arbitrary -- smtpConfigHost :: Text
    <*> arbitrary -- smtpConfigPassword :: Text
    <*> arbitrary -- smtpConfigPort :: Int
    <*> arbitraryReducedMaybe n -- smtpConfigTimeoutSeconds :: Maybe Integer
    <*> arbitrary -- smtpConfigUsername :: Text
  
instance Arbitrary StilleExportResponse where
  arbitrary = sized genStilleExportResponse

genStilleExportResponse :: Int -> Gen StilleExportResponse
genStilleExportResponse n =
  StilleExportResponse
    <$> arbitrary -- stilleExportResponseCsvContent :: Text
    <*> arbitrary -- stilleExportResponseFilename :: Text
  
instance Arbitrary StillePartnerZeile where
  arbitrary = sized genStillePartnerZeile

genStillePartnerZeile :: Int -> Gen StillePartnerZeile
genStillePartnerZeile n =
  StillePartnerZeile
    <$> arbitrary -- stillePartnerZeileAuseinandersetzungsguthaben :: Text
    <*> arbitrary -- stillePartnerZeileGewinnanteil :: Text
    <*> arbitrary -- stillePartnerZeileGewinnvortrag :: Text
    <*> arbitraryReducedMaybe n -- stillePartnerZeileHinweis :: Maybe Text
    <*> arbitrary -- stillePartnerZeileInstrumentType :: Text
    <*> arbitrary -- stillePartnerZeileKest :: Text
    <*> arbitrary -- stillePartnerZeileName :: Text
    <*> arbitrary -- stillePartnerZeileVerlustVerrechnungskonto :: Text
    <*> arbitrary -- stillePartnerZeileVerlustanteil :: Text
  
instance Arbitrary StilleReport where
  arbitrary = sized genStilleReport

genStilleReport :: Int -> Gen StilleReport
genStilleReport n =
  StilleReport
    <$> arbitrary -- stilleReportJahresueberschuss :: Text
    <*> arbitraryReduced n -- stilleReportPartners :: [StillePartnerZeile]
    <*> arbitrary -- stilleReportYear :: Int
  
instance Arbitrary StockAdjustment where
  arbitrary = sized genStockAdjustment

genStockAdjustment :: Int -> Gen StockAdjustment
genStockAdjustment n =
  StockAdjustment
    <$> arbitraryReducedMaybe n -- stockAdjustmentBatchNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockAdjustmentBinLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockAdjustmentExpiryDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- stockAdjustmentProductId :: Maybe Text
    <*> arbitrary -- stockAdjustmentQuantity :: Integer
    <*> arbitraryReducedMaybe n -- stockAdjustmentSerialNumbers :: Maybe [Text]
  
instance Arbitrary StockMovement where
  arbitrary = sized genStockMovement

genStockMovement :: Int -> Gen StockMovement
genStockMovement n =
  StockMovement
    <$> arbitrary -- stockMovementDelta :: Integer
    <*> arbitraryReduced n -- stockMovementMovementType :: MovementType
    <*> arbitrary -- stockMovementProductId :: Text
    <*> arbitrary -- stockMovementQuantity :: Integer
    <*> arbitraryReducedMaybe n -- stockMovementReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockMovementReferenceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- stockMovementReferenceType :: Maybe ReferenceType
    <*> arbitrary -- stockMovementWarehouseId :: Text
  
instance Arbitrary StockTransfer where
  arbitrary = sized genStockTransfer

genStockTransfer :: Int -> Gen StockTransfer
genStockTransfer n =
  StockTransfer
    <$> arbitraryReduced n -- stockTransferLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- stockTransferNotes :: Maybe Text
    <*> arbitrary -- stockTransferSourceWarehouseId :: Text
    <*> arbitraryReduced n -- stockTransferStatus :: StockTransferStatus
    <*> arbitrary -- stockTransferTargetWarehouseId :: Text
    <*> arbitraryReduced n -- stockTransferTransferDate :: Date
    <*> arbitrary -- stockTransferTransferNumber :: Text
  
instance Arbitrary StockTransferStatusUpdate where
  arbitrary = sized genStockTransferStatusUpdate

genStockTransferStatusUpdate :: Int -> Gen StockTransferStatusUpdate
genStockTransferStatusUpdate n =
  StockTransferStatusUpdate
    <$> arbitrary -- stockTransferStatusUpdateStatus :: Text
  
instance Arbitrary StockUpdateRequest where
  arbitrary = sized genStockUpdateRequest

genStockUpdateRequest :: Int -> Gen StockUpdateRequest
genStockUpdateRequest n =
  StockUpdateRequest
    <$> arbitrary -- stockUpdateRequestQuantity :: Integer
  
instance Arbitrary SubmitResultDto where
  arbitrary = sized genSubmitResultDto

genSubmitResultDto :: Int -> Gen SubmitResultDto
genSubmitResultDto n =
  SubmitResultDto
    <$> arbitrary -- submitResultDtoAnswers :: [Int]
    <*> arbitraryReducedMaybe n -- submitResultDtoAssignmentId :: Maybe Text
    <*> arbitrary -- submitResultDtoScore :: Int
    <*> arbitrary -- submitResultDtoTrainingCode :: Text
  
instance Arbitrary SubmitResultResponse where
  arbitrary = sized genSubmitResultResponse

genSubmitResultResponse :: Int -> Gen SubmitResultResponse
genSubmitResultResponse n =
  SubmitResultResponse
    <$> arbitraryReducedMaybe n -- submitResultResponseCertificateId :: Maybe Text
    <*> arbitrary -- submitResultResponseCompletionId :: Text
    <*> arbitrary -- submitResultResponsePassScore :: Int
    <*> arbitrary -- submitResultResponsePassed :: Bool
    <*> arbitrary -- submitResultResponseScore :: Int
    <*> arbitraryReducedMaybe n -- submitResultResponseValidUntil :: Maybe DateTime
  
instance Arbitrary SubscriptionOverview where
  arbitrary = sized genSubscriptionOverview

genSubscriptionOverview :: Int -> Gen SubscriptionOverview
genSubscriptionOverview n =
  SubscriptionOverview
    <$> arbitraryReducedMaybe n -- subscriptionOverviewCurrentPeriodEnd :: Maybe DateTime
    <*> arbitraryReduced n -- subscriptionOverviewFeatures :: PlanFeatures
    <*> arbitrary -- subscriptionOverviewIsTrialing :: Bool
    <*> arbitraryReduced n -- subscriptionOverviewLimits :: PlanLimits
    <*> arbitraryReducedMaybe n -- subscriptionOverviewManageUrl :: Maybe Text
    <*> arbitrary -- subscriptionOverviewPlan :: Text
    <*> arbitrary -- subscriptionOverviewPlanName :: Text
    <*> arbitrary -- subscriptionOverviewPriceEur :: Double
    <*> arbitraryReducedMaybe n -- subscriptionOverviewQuantity :: Maybe Int
    <*> arbitraryReducedMaybe n -- subscriptionOverviewStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- subscriptionOverviewSubscriptionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- subscriptionOverviewTrialEndsAt :: Maybe DateTime
    <*> arbitraryReduced n -- subscriptionOverviewUsage :: UsageSnapshot
  
instance Arbitrary SuitabilityRequest where
  arbitrary = sized genSuitabilityRequest

genSuitabilityRequest :: Int -> Gen SuitabilityRequest
genSuitabilityRequest n =
  SuitabilityRequest
    <$> arbitraryReducedMaybe n -- suitabilityRequestCustomerAnnualVolume :: Maybe Int
    <*> arbitraryReduced n -- suitabilityRequestItems :: [CartItemInput]
    <*> arbitraryReduced n -- suitabilityRequestRecipient :: Address
    <*> arbitraryReduced n -- suitabilityRequestSender :: Address
  
instance Arbitrary SuitabilityResult where
  arbitrary = sized genSuitabilityResult

genSuitabilityResult :: Int -> Gen SuitabilityResult
genSuitabilityResult n =
  SuitabilityResult
    <$> arbitraryReduced n -- suitabilityResultMethods :: [MethodSuitability]
    <*> arbitraryReducedMaybe n -- suitabilityResultRecommendedBox :: Maybe BoxFit
    <*> arbitrary -- suitabilityResultRequiresInsurance :: Bool
    <*> arbitrary -- suitabilityResultTotalValue :: Text
    <*> arbitrary -- suitabilityResultTotalWeightKg :: Double
  
instance Arbitrary SupplierCondition where
  arbitrary = sized genSupplierCondition

genSupplierCondition :: Int -> Gen SupplierCondition
genSupplierCondition n =
  SupplierCondition
    <$> arbitrary -- supplierConditionCurrency :: Text
    <*> arbitraryReducedMaybe n -- supplierConditionDeliveryTerms :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionEarlyPaymentDiscountPercent :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionIsDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- supplierConditionMinimumOrderValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionPaymentDueDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- supplierConditionPaymentTerms :: Maybe Text
    <*> arbitrary -- supplierConditionSupplierContactId :: Text
    <*> arbitraryReducedMaybe n -- supplierConditionSupplierName :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionVolumeDiscountTiers :: Maybe AnyType
  
instance Arbitrary SupplierConditionCreate where
  arbitrary = sized genSupplierConditionCreate

genSupplierConditionCreate :: Int -> Gen SupplierConditionCreate
genSupplierConditionCreate n =
  SupplierConditionCreate
    <$> arbitrary -- supplierConditionCreateCurrency :: Text
    <*> arbitraryReducedMaybe n -- supplierConditionCreateDeliveryTerms :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionCreateEarlyPaymentDiscountPercent :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionCreateIsDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- supplierConditionCreateMinimumOrderValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionCreatePaymentDueDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- supplierConditionCreatePaymentTerms :: Maybe Text
    <*> arbitrary -- supplierConditionCreateSupplierContactId :: Text
    <*> arbitraryReducedMaybe n -- supplierConditionCreateSupplierName :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionCreateVolumeDiscountTiers :: Maybe AnyType
  
instance Arbitrary SupplierConditionUpdate where
  arbitrary = sized genSupplierConditionUpdate

genSupplierConditionUpdate :: Int -> Gen SupplierConditionUpdate
genSupplierConditionUpdate n =
  SupplierConditionUpdate
    <$> arbitraryReducedMaybe n -- supplierConditionUpdateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionUpdateDeliveryTerms :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionUpdateEarlyPaymentDiscountPercent :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionUpdateIsDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- supplierConditionUpdateMinimumOrderValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionUpdatePaymentDueDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- supplierConditionUpdatePaymentTerms :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionUpdateSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionUpdateSupplierName :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierConditionUpdateVolumeDiscountTiers :: Maybe AnyType
  
instance Arbitrary SupplierInvoice where
  arbitrary = sized genSupplierInvoice

genSupplierInvoice :: Int -> Gen SupplierInvoice
genSupplierInvoice n =
  SupplierInvoice
    <$> arbitraryReducedMaybe n -- supplierInvoiceCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceGoodsReceiptId :: Maybe Text
    <*> arbitraryReduced n -- supplierInvoiceInvoiceDate :: Date
    <*> arbitrary -- supplierInvoiceInvoiceNumber :: Text
    <*> arbitraryReduced n -- supplierInvoiceLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- supplierInvoiceNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoicePurchaseOrderId :: Maybe Text
    <*> arbitraryReduced n -- supplierInvoiceStatus :: SupplierInvoiceStatus
    <*> arbitraryReducedMaybe n -- supplierInvoiceSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceSupplierName :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceTotalGrossAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceTotalNetAmount :: Maybe Text
  
instance Arbitrary SupplierInvoiceCreate where
  arbitrary = sized genSupplierInvoiceCreate

genSupplierInvoiceCreate :: Int -> Gen SupplierInvoiceCreate
genSupplierInvoiceCreate n =
  SupplierInvoiceCreate
    <$> arbitraryReducedMaybe n -- supplierInvoiceCreateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceCreateGoodsReceiptId :: Maybe Text
    <*> arbitraryReduced n -- supplierInvoiceCreateInvoiceDate :: Date
    <*> arbitrary -- supplierInvoiceCreateInvoiceNumber :: Text
    <*> arbitraryReduced n -- supplierInvoiceCreateLineItems :: AnyType
    <*> arbitraryReducedMaybe n -- supplierInvoiceCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceCreatePurchaseOrderId :: Maybe Text
    <*> arbitraryReduced n -- supplierInvoiceCreateStatus :: SupplierInvoiceStatus
    <*> arbitraryReducedMaybe n -- supplierInvoiceCreateSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceCreateSupplierName :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceCreateTotalGrossAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceCreateTotalNetAmount :: Maybe Text
  
instance Arbitrary SupplierInvoiceStatusUpdate where
  arbitrary = sized genSupplierInvoiceStatusUpdate

genSupplierInvoiceStatusUpdate :: Int -> Gen SupplierInvoiceStatusUpdate
genSupplierInvoiceStatusUpdate n =
  SupplierInvoiceStatusUpdate
    <$> arbitrary -- supplierInvoiceStatusUpdateStatus :: Text
  
instance Arbitrary SupplierInvoiceUpdate where
  arbitrary = sized genSupplierInvoiceUpdate

genSupplierInvoiceUpdate :: Int -> Gen SupplierInvoiceUpdate
genSupplierInvoiceUpdate n =
  SupplierInvoiceUpdate
    <$> arbitraryReducedMaybe n -- supplierInvoiceUpdateCurrency :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateGoodsReceiptId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateInvoiceDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateInvoiceNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdatePurchaseOrderId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateStatus :: Maybe SupplierInvoiceStatus
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateSupplierContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateSupplierName :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateTotalGrossAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- supplierInvoiceUpdateTotalNetAmount :: Maybe Text
  
instance Arbitrary SupportChannel where
  arbitrary = sized genSupportChannel

genSupportChannel :: Int -> Gen SupportChannel
genSupportChannel n =
  SupportChannel
    <$> arbitraryReduced n -- supportChannelChannelType :: SupportChannelType
    <*> arbitraryReduced n -- supportChannelConfig :: AnyType
    <*> arbitraryReduced n -- supportChannelCreatedAt :: DateTime
    <*> arbitrary -- supportChannelIsActive :: Bool
    <*> arbitrary -- supportChannelName :: Text
    <*> arbitrary -- supportChannelTenantId :: Text
    <*> arbitraryReducedMaybe n -- supportChannelUpdatedAt :: Maybe DateTime
  
instance Arbitrary SupportTicket where
  arbitrary = sized genSupportTicket

genSupportTicket :: Int -> Gen SupportTicket
genSupportTicket n =
  SupportTicket
    <$> arbitraryReducedMaybe n -- supportTicketAssignedTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketChannelId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketChannelType :: Maybe SupportChannelType
    <*> arbitraryReducedMaybe n -- supportTicketClosedAt :: Maybe DateTime
    <*> arbitraryReduced n -- supportTicketCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- supportTicketCustomerEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketExternalId :: Maybe Text
    <*> arbitraryReduced n -- supportTicketFirstMessageAt :: DateTime
    <*> arbitraryReduced n -- supportTicketLastMessageAt :: DateTime
    <*> arbitraryReducedMaybe n -- supportTicketLeadId :: Maybe Text
    <*> arbitrary -- supportTicketMessageCount :: Int
    <*> arbitraryReducedMaybe n -- supportTicketOrderRef :: Maybe Text
    <*> arbitraryReduced n -- supportTicketPriority :: TicketPriority
    <*> arbitraryReducedMaybe n -- supportTicketResolution :: Maybe Text
    <*> arbitraryReduced n -- supportTicketStatus :: SupportTicketStatus
    <*> arbitrary -- supportTicketSubject :: Text
    <*> arbitraryReduced n -- supportTicketTags :: AnyType
    <*> arbitrary -- supportTicketTenantId :: Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdatedAt :: Maybe DateTime
  
instance Arbitrary SupportTicketUpdate where
  arbitrary = sized genSupportTicketUpdate

genSupportTicketUpdate :: Int -> Gen SupportTicketUpdate
genSupportTicketUpdate n =
  SupportTicketUpdate
    <$> arbitraryReducedMaybe n -- supportTicketUpdateAssignedTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateChannelId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateChannelType :: Maybe SupportChannelType
    <*> arbitraryReducedMaybe n -- supportTicketUpdateClosedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- supportTicketUpdateCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- supportTicketUpdateCustomerEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateCustomerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateExternalId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateFirstMessageAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- supportTicketUpdateLastMessageAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- supportTicketUpdateLeadId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateMessageCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- supportTicketUpdateOrderRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdatePriority :: Maybe TicketPriority
    <*> arbitraryReducedMaybe n -- supportTicketUpdateResolution :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateStatus :: Maybe SupportTicketStatus
    <*> arbitraryReducedMaybe n -- supportTicketUpdateSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateTags :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- supportTicketUpdateTenantId :: Maybe Text
    <*> arbitraryReducedMaybe n -- supportTicketUpdateUpdatedAt :: Maybe DateTime
  
instance Arbitrary SyncLog where
  arbitrary = sized genSyncLog

genSyncLog :: Int -> Gen SyncLog
genSyncLog n =
  SyncLog
    <$> arbitraryReducedMaybe n -- syncLogCompletedAt :: Maybe DateTime
    <*> arbitrary -- syncLogConnectionId :: Text
    <*> arbitraryReducedMaybe n -- syncLogErrorMessage :: Maybe Text
    <*> arbitrary -- syncLogItemsFailed :: Int
    <*> arbitrary -- syncLogItemsSynced :: Int
    <*> arbitrary -- syncLogLogId :: Text
    <*> arbitrary -- syncLogPlatform :: Text
    <*> arbitraryReduced n -- syncLogStartedAt :: DateTime
    <*> arbitrary -- syncLogStatus :: Text
    <*> arbitrary -- syncLogSyncType :: Text
  
instance Arbitrary SyncSummary where
  arbitrary = sized genSyncSummary

genSyncSummary :: Int -> Gen SyncSummary
genSyncSummary n =
  SyncSummary
    <$> arbitraryReducedMaybe n -- syncSummaryErrorMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- syncSummaryItemsFailed :: Maybe Int
    <*> arbitraryReducedMaybe n -- syncSummaryItemsSynced :: Maybe Int
  
instance Arbitrary TargetProgress where
  arbitrary = sized genTargetProgress

genTargetProgress :: Int -> Gen TargetProgress
genTargetProgress n =
  TargetProgress
    <$> arbitrary -- targetProgressBaseValue :: Double
    <*> arbitrary -- targetProgressBaseYear :: Int
    <*> arbitrary -- targetProgressDescription :: Text
    <*> arbitrary -- targetProgressId :: Text
    <*> arbitraryReducedMaybe n -- targetProgressProgressPct :: Maybe Double
    <*> arbitrary -- targetProgressScope :: Text
    <*> arbitrary -- targetProgressTargetValue :: Double
    <*> arbitrary -- targetProgressTargetYear :: Int
  
instance Arbitrary TaxRateCreate where
  arbitrary = sized genTaxRateCreate

genTaxRateCreate :: Int -> Gen TaxRateCreate
genTaxRateCreate n =
  TaxRateCreate
    <$> arbitrary -- taxRateCreateCountryCode :: Text
    <*> arbitraryReducedMaybe n -- taxRateCreateEffectiveFrom :: Maybe Date
    <*> arbitrary -- taxRateCreateIsDefault :: Bool
    <*> arbitrary -- taxRateCreateName :: Text
    <*> arbitrary -- taxRateCreateRatePercent :: Integer
  
instance Arbitrary Team where
  arbitrary = sized genTeam

genTeam :: Int -> Gen Team
genTeam n =
  Team
    <$> arbitraryReduced n -- teamCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- teamDescription :: Maybe Text
    <*> arbitrary -- teamId :: Text
    <*> arbitrary -- teamName :: Text
    <*> arbitraryReducedMaybe n -- teamParentTeamId :: Maybe Text
    <*> arbitrary -- teamTenantId :: Text
    <*> arbitraryReduced n -- teamUpdatedAt :: DateTime
  
instance Arbitrary TeamCreate where
  arbitrary = sized genTeamCreate

genTeamCreate :: Int -> Gen TeamCreate
genTeamCreate n =
  TeamCreate
    <$> arbitraryReducedMaybe n -- teamCreateDescription :: Maybe Text
    <*> arbitrary -- teamCreateName :: Text
    <*> arbitraryReducedMaybe n -- teamCreateParentTeamId :: Maybe Text
  
instance Arbitrary TenantSettings where
  arbitrary = sized genTenantSettings

genTenantSettings :: Int -> Gen TenantSettings
genTenantSettings n =
  TenantSettings
    <$> arbitraryReduced n -- tenantSettingsCompanyType :: CompanyType
    <*> arbitraryReducedMaybe n -- tenantSettingsDpaAcceptedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- tenantSettingsDpaAcceptedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- tenantSettingsDpaVersion :: Maybe Text
    <*> arbitraryReduced n -- tenantSettingsFeatures :: AnyType
  
instance Arbitrary TenantUser where
  arbitrary = sized genTenantUser

genTenantUser :: Int -> Gen TenantUser
genTenantUser n =
  TenantUser
    <$> arbitrary -- tenantUserEmail :: Text
    <*> arbitrary -- tenantUserEmailVerified :: Bool
    <*> arbitrary -- tenantUserIsActive :: Bool
    <*> arbitraryReduced n -- tenantUserJoinedAt :: DateTime
    <*> arbitraryReducedMaybe n -- tenantUserLastLogin :: Maybe DateTime
    <*> arbitrary -- tenantUserName :: Text
    <*> arbitrary -- tenantUserPermissions :: [Text]
    <*> arbitrary -- tenantUserRole :: Text
    <*> arbitrary -- tenantUserUserId :: Text
  
instance Arbitrary TicketMessage where
  arbitrary = sized genTicketMessage

genTicketMessage :: Int -> Gen TicketMessage
genTicketMessage n =
  TicketMessage
    <$> arbitraryReducedMaybe n -- ticketMessageAuthorEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- ticketMessageAuthorName :: Maybe Text
    <*> arbitrary -- ticketMessageBody :: Text
    <*> arbitraryReducedMaybe n -- ticketMessageBodyHtml :: Maybe Text
    <*> arbitraryReducedMaybe n -- ticketMessageChannelId :: Maybe Text
    <*> arbitraryReduced n -- ticketMessageCreatedAt :: DateTime
    <*> arbitraryReduced n -- ticketMessageDirection :: MessageDirection
    <*> arbitraryReducedMaybe n -- ticketMessageExternalId :: Maybe Text
    <*> arbitrary -- ticketMessageIsInternal :: Bool
    <*> arbitraryReduced n -- ticketMessageMessageType :: MessageType
    <*> arbitraryReduced n -- ticketMessageMetadata :: AnyType
    <*> arbitrary -- ticketMessageTenantId :: Text
    <*> arbitrary -- ticketMessageTicketId :: Text
  
instance Arbitrary TimeEntryClockIn where
  arbitrary = sized genTimeEntryClockIn

genTimeEntryClockIn :: Int -> Gen TimeEntryClockIn
genTimeEntryClockIn n =
  TimeEntryClockIn
    <$> arbitraryReducedMaybe n -- timeEntryClockInNotes :: Maybe Text
  
instance Arbitrary TimeEntryClockOut where
  arbitrary = sized genTimeEntryClockOut

genTimeEntryClockOut :: Int -> Gen TimeEntryClockOut
genTimeEntryClockOut n =
  TimeEntryClockOut
    <$> arbitraryReduced n -- timeEntryClockOutClockOut :: DateTime
    <*> arbitraryReducedMaybe n -- timeEntryClockOutHours :: Maybe Text
  
instance Arbitrary TimeEntryDto where
  arbitrary = sized genTimeEntryDto

genTimeEntryDto :: Int -> Gen TimeEntryDto
genTimeEntryDto n =
  TimeEntryDto
    <$> arbitraryReducedMaybe n -- timeEntryDtoClockIn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- timeEntryDtoClockOut :: Maybe DateTime
    <*> arbitraryReduced n -- timeEntryDtoCreatedAt :: DateTime
    <*> arbitraryReduced n -- timeEntryDtoDate :: Date
    <*> arbitrary -- timeEntryDtoEmployeeId :: Text
    <*> arbitraryReducedMaybe n -- timeEntryDtoHours :: Maybe Text
    <*> arbitraryReducedMaybe n -- timeEntryDtoNotes :: Maybe Text
    <*> arbitrary -- timeEntryDtoTimeEntryId :: Text
  
instance Arbitrary TimelineEvent where
  arbitrary = sized genTimelineEvent

genTimelineEvent :: Int -> Gen TimelineEvent
genTimelineEvent n =
  TimelineEvent
    <$> arbitrary -- timelineEventDate :: Text
    <*> arbitraryReducedMaybe n -- timelineEventDetail :: Maybe Text
    <*> arbitrary -- timelineEventId :: Text
    <*> arbitraryReducedMaybe n -- timelineEventStatus :: Maybe Text
    <*> arbitrary -- timelineEventTitle :: Text
    <*> arbitrary -- timelineEventType :: Text
  
instance Arbitrary TotpEnableRequest where
  arbitrary = sized genTotpEnableRequest

genTotpEnableRequest :: Int -> Gen TotpEnableRequest
genTotpEnableRequest n =
  TotpEnableRequest
    <$> arbitrary -- totpEnableRequestCode :: Text
  
instance Arbitrary TotpSetupResponse where
  arbitrary = sized genTotpSetupResponse

genTotpSetupResponse :: Int -> Gen TotpSetupResponse
genTotpSetupResponse n =
  TotpSetupResponse
    <$> arbitrary -- totpSetupResponseBackupCodes :: [Text]
    <*> arbitrary -- totpSetupResponseQrCodeUrl :: Text
    <*> arbitrary -- totpSetupResponseSecret :: Text
  
instance Arbitrary TrackOrderRequest where
  arbitrary = sized genTrackOrderRequest

genTrackOrderRequest :: Int -> Gen TrackOrderRequest
genTrackOrderRequest n =
  TrackOrderRequest
    <$> arbitrary -- trackOrderRequestEmail :: Text
    <*> arbitrary -- trackOrderRequestOrderNumber :: Text
  
instance Arbitrary TrackOrderResponse where
  arbitrary = sized genTrackOrderResponse

genTrackOrderResponse :: Int -> Gen TrackOrderResponse
genTrackOrderResponse n =
  TrackOrderResponse
    <$> arbitrary -- trackOrderResponseOrderNumber :: Text
    <*> arbitrary -- trackOrderResponseOrderStatus :: Text
    <*> arbitraryReduced n -- trackOrderResponseShipments :: [TrackedShipment]
  
instance Arbitrary TrackedShipment where
  arbitrary = sized genTrackedShipment

genTrackedShipment :: Int -> Gen TrackedShipment
genTrackedShipment n =
  TrackedShipment
    <$> arbitrary -- trackedShipmentCarrier :: Text
    <*> arbitraryReduced n -- trackedShipmentEvents :: [TrackingEvent]
    <*> arbitraryReducedMaybe n -- trackedShipmentLabelUrl :: Maybe Text
    <*> arbitrary -- trackedShipmentStatus :: Text
    <*> arbitraryReducedMaybe n -- trackedShipmentTrackingNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- trackedShipmentTrackingUrl :: Maybe Text
  
instance Arbitrary TrackingEvent where
  arbitrary = sized genTrackingEvent

genTrackingEvent :: Int -> Gen TrackingEvent
genTrackingEvent n =
  TrackingEvent
    <$> arbitrary -- trackingEventDate :: Text
    <*> arbitrary -- trackingEventDescription :: Text
    <*> arbitrary -- trackingEventLocation :: Text
    <*> arbitrary -- trackingEventStatus :: Text
  
instance Arbitrary TrackingInfo where
  arbitrary = sized genTrackingInfo

genTrackingInfo :: Int -> Gen TrackingInfo
genTrackingInfo n =
  TrackingInfo
    <$> arbitrary -- trackingInfoCarrier :: Text
    <*> arbitraryReducedMaybe n -- trackingInfoEstimatedDelivery :: Maybe Text
    <*> arbitraryReduced n -- trackingInfoEvents :: [TrackingEvent]
    <*> arbitraryReducedMaybe n -- trackingInfoRawResponse :: Maybe AnyType
    <*> arbitrary -- trackingInfoStatus :: Text
    <*> arbitrary -- trackingInfoTrackingNumber :: Text
  
instance Arbitrary TrainingAssignment where
  arbitrary = sized genTrainingAssignment

genTrainingAssignment :: Int -> Gen TrainingAssignment
genTrainingAssignment n =
  TrainingAssignment
    <$> arbitraryReducedMaybe n -- trainingAssignmentAssignedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- trainingAssignmentDeletedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- trainingAssignmentDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- trainingAssignmentEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentId :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentStatus :: Maybe AssignmentStatus
    <*> arbitraryReducedMaybe n -- trainingAssignmentTenantId :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentTrainingId :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentUpdatedAt :: Maybe DateTime
  
instance Arbitrary TrainingAssignmentCreate where
  arbitrary = sized genTrainingAssignmentCreate

genTrainingAssignmentCreate :: Int -> Gen TrainingAssignmentCreate
genTrainingAssignmentCreate n =
  TrainingAssignmentCreate
    <$> arbitraryReducedMaybe n -- trainingAssignmentCreateAssignedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentCreateDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- trainingAssignmentCreateEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentCreateStatus :: Maybe AssignmentStatus
    <*> arbitraryReducedMaybe n -- trainingAssignmentCreateTrainingId :: Maybe Text
  
instance Arbitrary TrainingAssignmentUpdate where
  arbitrary = sized genTrainingAssignmentUpdate

genTrainingAssignmentUpdate :: Int -> Gen TrainingAssignmentUpdate
genTrainingAssignmentUpdate n =
  TrainingAssignmentUpdate
    <$> arbitraryReducedMaybe n -- trainingAssignmentUpdateAssignedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentUpdateDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- trainingAssignmentUpdateEmployeeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentUpdateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- trainingAssignmentUpdateStatus :: Maybe AssignmentStatus
    <*> arbitraryReducedMaybe n -- trainingAssignmentUpdateTrainingId :: Maybe Text
  
instance Arbitrary TrainingContent where
  arbitrary = sized genTrainingContent

genTrainingContent :: Int -> Gen TrainingContent
genTrainingContent n =
  TrainingContent
    <$> arbitrary -- trainingContentCode :: Text
    <*> arbitraryReduced n -- trainingContentContact :: ContactInfo
    <*> arbitrary -- trainingContentPassScore :: Int
    <*> arbitraryReduced n -- trainingContentQuiz :: [QuizQuestion]
    <*> arbitraryReduced n -- trainingContentSections :: [Section]
    <*> arbitrary -- trainingContentTitle :: Text
    <*> arbitrary -- trainingContentTitleEn :: Text
  
instance Arbitrary UmsatzsteuerReport where
  arbitrary = sized genUmsatzsteuerReport

genUmsatzsteuerReport :: Int -> Gen UmsatzsteuerReport
genUmsatzsteuerReport n =
  UmsatzsteuerReport
    <$> arbitrary -- umsatzsteuerReportGeneratedAt :: Text
    <*> arbitraryReduced n -- umsatzsteuerReportInputTax :: [VatDetail]
    <*> arbitraryReduced n -- umsatzsteuerReportOutputTax :: [VatDetail]
    <*> arbitrary -- umsatzsteuerReportPeriod :: Text
    <*> arbitrary -- umsatzsteuerReportTotalInputTax :: Text
    <*> arbitrary -- umsatzsteuerReportTotalOutputTax :: Text
    <*> arbitrary -- umsatzsteuerReportVatPayable :: Text
    <*> arbitrary -- umsatzsteuerReportVatRefund :: Text
  
instance Arbitrary UpdateAutomation where
  arbitrary = sized genUpdateAutomation

genUpdateAutomation :: Int -> Gen UpdateAutomation
genUpdateAutomation n =
  UpdateAutomation
    <$> arbitraryReducedMaybe n -- updateAutomationConfig :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- updateAutomationEnabled :: Maybe Bool
  
instance Arbitrary UpdateChannelDto where
  arbitrary = sized genUpdateChannelDto

genUpdateChannelDto :: Int -> Gen UpdateChannelDto
genUpdateChannelDto n =
  UpdateChannelDto
    <$> arbitraryReducedMaybe n -- updateChannelDtoConfig :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- updateChannelDtoIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateChannelDtoName :: Maybe Text
  
instance Arbitrary UpdateConnectionRequest where
  arbitrary = sized genUpdateConnectionRequest

genUpdateConnectionRequest :: Int -> Gen UpdateConnectionRequest
genUpdateConnectionRequest n =
  UpdateConnectionRequest
    <$> arbitraryReducedMaybe n -- updateConnectionRequestApiKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateConnectionRequestApiSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateConnectionRequestConfig :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- updateConnectionRequestIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateConnectionRequestLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateConnectionRequestShopDomain :: Maybe Text
  
instance Arbitrary UpdatePermissionsPayload where
  arbitrary = sized genUpdatePermissionsPayload

genUpdatePermissionsPayload :: Int -> Gen UpdatePermissionsPayload
genUpdatePermissionsPayload n =
  UpdatePermissionsPayload
    <$> arbitrary -- updatePermissionsPayloadPermissions :: [Text]
  
instance Arbitrary UpdateProfileRequest where
  arbitrary = sized genUpdateProfileRequest

genUpdateProfileRequest :: Int -> Gen UpdateProfileRequest
genUpdateProfileRequest n =
  UpdateProfileRequest
    <$> arbitraryReducedMaybe n -- updateProfileRequestAvatarUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateProfileRequestFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateProfileRequestLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateProfileRequestName :: Maybe Text
  
instance Arbitrary UpdateRolePayload where
  arbitrary = sized genUpdateRolePayload

genUpdateRolePayload :: Int -> Gen UpdateRolePayload
genUpdateRolePayload n =
  UpdateRolePayload
    <$> arbitrary -- updateRolePayloadRole :: Text
    <*> arbitraryReducedMaybe n -- updateRolePayloadSyncPermissions :: Maybe Bool
  
instance Arbitrary UpdateSubscriptionRequest where
  arbitrary = sized genUpdateSubscriptionRequest

genUpdateSubscriptionRequest :: Int -> Gen UpdateSubscriptionRequest
genUpdateSubscriptionRequest n =
  UpdateSubscriptionRequest
    <$> arbitraryReducedMaybe n -- updateSubscriptionRequestEventType :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSubscriptionRequestIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateSubscriptionRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSubscriptionRequestSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSubscriptionRequestUrl :: Maybe Text
  
instance Arbitrary UpdateSyncDirectionRequest where
  arbitrary = sized genUpdateSyncDirectionRequest

genUpdateSyncDirectionRequest :: Int -> Gen UpdateSyncDirectionRequest
genUpdateSyncDirectionRequest n =
  UpdateSyncDirectionRequest
    <$> arbitrary -- updateSyncDirectionRequestDirections :: (Map.Map String Text)
  
instance Arbitrary UpdateTenantSettings where
  arbitrary = sized genUpdateTenantSettings

genUpdateTenantSettings :: Int -> Gen UpdateTenantSettings
genUpdateTenantSettings n =
  UpdateTenantSettings
    <$> arbitraryReduced n -- updateTenantSettingsCompanyType :: CompanyType
    <*> arbitraryReducedMaybe n -- updateTenantSettingsFeatures :: Maybe PartialFeatureSettings
  
instance Arbitrary UpsCredentials where
  arbitrary = sized genUpsCredentials

genUpsCredentials :: Int -> Gen UpsCredentials
genUpsCredentials n =
  UpsCredentials
    <$> arbitrary -- upsCredentialsClientId :: Text
    <*> arbitrary -- upsCredentialsClientSecret :: Text
    <*> arbitraryReducedMaybe n -- upsCredentialsShipperNumber :: Maybe Text
  
instance Arbitrary UsageSnapshot where
  arbitrary = sized genUsageSnapshot

genUsageSnapshot :: Int -> Gen UsageSnapshot
genUsageSnapshot n =
  UsageSnapshot
    <$> arbitrary -- usageSnapshotConnectors :: Integer
    <*> arbitrary -- usageSnapshotInvoicesThisMonth :: Integer
    <*> arbitrary -- usageSnapshotOverageSeats :: Integer
    <*> arbitrary -- usageSnapshotUsers :: Integer
  
instance Arbitrary UserProfile where
  arbitrary = sized genUserProfile

genUserProfile :: Int -> Gen UserProfile
genUserProfile n =
  UserProfile
    <$> arbitraryReduced n -- userProfileCreatedAt :: DateTime
    <*> arbitrary -- userProfileEmail :: Text
    <*> arbitrary -- userProfileEmailVerified :: Bool
    <*> arbitrary -- userProfileFirstName :: Text
    <*> arbitrary -- userProfileFullName :: Text
    <*> arbitrary -- userProfileId :: Text
    <*> arbitrary -- userProfileLastName :: Text
  
instance Arbitrary UserTenantInfo where
  arbitrary = sized genUserTenantInfo

genUserTenantInfo :: Int -> Gen UserTenantInfo
genUserTenantInfo n =
  UserTenantInfo
    <$> arbitraryReducedMaybe n -- userTenantInfoCustomDomain :: Maybe Text
    <*> arbitrary -- userTenantInfoRole :: Text
    <*> arbitraryReducedMaybe n -- userTenantInfoSubdomain :: Maybe Text
    <*> arbitrary -- userTenantInfoTenantId :: Text
    <*> arbitrary -- userTenantInfoTenantName :: Text
  
instance Arbitrary UstvaErgebnis where
  arbitrary = sized genUstvaErgebnis

genUstvaErgebnis :: Int -> Gen UstvaErgebnis
genUstvaErgebnis n =
  UstvaErgebnis
    <$> arbitrary -- ustvaErgebnisBis :: Text
    <*> arbitraryReducedMaybe n -- ustvaErgebnisHinweis :: Maybe Text
    <*> arbitrary -- ustvaErgebnisIstKleinunternehmer :: Bool
    <*> arbitrary -- ustvaErgebnisKz41 :: Text
    <*> arbitrary -- ustvaErgebnisKz43 :: Text
    <*> arbitrary -- ustvaErgebnisKz46 :: Text
    <*> arbitrary -- ustvaErgebnisKz47 :: Text
    <*> arbitrary -- ustvaErgebnisKz61 :: Text
    <*> arbitrary -- ustvaErgebnisKz66 :: Text
    <*> arbitrary -- ustvaErgebnisKz67 :: Text
    <*> arbitrary -- ustvaErgebnisKz81 :: Text
    <*> arbitrary -- ustvaErgebnisKz83 :: Text
    <*> arbitrary -- ustvaErgebnisKz84 :: Text
    <*> arbitrary -- ustvaErgebnisKz85 :: Text
    <*> arbitrary -- ustvaErgebnisKz86 :: Text
    <*> arbitrary -- ustvaErgebnisKz88 :: Text
    <*> arbitrary -- ustvaErgebnisKz89 :: Text
    <*> arbitrary -- ustvaErgebnisKz93 :: Text
    <*> arbitrary -- ustvaErgebnisVon :: Text
    <*> arbitrary -- ustvaErgebnisZahllast :: Text
    <*> arbitrary -- ustvaErgebnisZeitraum :: Text
    <*> arbitrary -- ustvaErgebnisZeitraumTyp :: Text
  
instance Arbitrary VatDetail where
  arbitrary = sized genVatDetail

genVatDetail :: Int -> Gen VatDetail
genVatDetail n =
  VatDetail
    <$> arbitrary -- vatDetailCount :: Integer
    <*> arbitrary -- vatDetailNetAmount :: Text
    <*> arbitrary -- vatDetailTaxAmount :: Text
    <*> arbitrary -- vatDetailTaxRate :: Text
  
instance Arbitrary VatItem where
  arbitrary = sized genVatItem

genVatItem :: Int -> Gen VatItem
genVatItem n =
  VatItem
    <$> arbitrary -- vatItemNetAmount :: Text
    <*> arbitrary -- vatItemTaxAmount :: Text
    <*> arbitrary -- vatItemTaxRate :: Text
  
instance Arbitrary VatSummary where
  arbitrary = sized genVatSummary

genVatSummary :: Int -> Gen VatSummary
genVatSummary n =
  VatSummary
    <$> arbitraryReduced n -- vatSummaryInputTaxItems :: [VatItem]
    <*> arbitraryReduced n -- vatSummaryOutputTaxItems :: [VatItem]
    <*> arbitrary -- vatSummaryTotalInputTax :: Text
    <*> arbitrary -- vatSummaryTotalOutputTax :: Text
    <*> arbitrary -- vatSummaryVatDue :: Text
  
instance Arbitrary Verfahrensdokumentation where
  arbitrary = sized genVerfahrensdokumentation

genVerfahrensdokumentation :: Int -> Gen Verfahrensdokumentation
genVerfahrensdokumentation n =
  Verfahrensdokumentation
    <$> arbitraryReduced n -- verfahrensdokumentationEntries :: [ComplianceEntry]
    <*> arbitrary -- verfahrensdokumentationGeneratedAt :: Text
    <*> arbitrary -- verfahrensdokumentationTitle :: Text
    <*> arbitrary -- verfahrensdokumentationVersion :: Text
  
instance Arbitrary VerifyEmailRequest where
  arbitrary = sized genVerifyEmailRequest

genVerifyEmailRequest :: Int -> Gen VerifyEmailRequest
genVerifyEmailRequest n =
  VerifyEmailRequest
    <$> arbitrary -- verifyEmailRequestToken :: Text
  
instance Arbitrary Voucher where
  arbitrary = sized genVoucher

genVoucher :: Int -> Gen Voucher
genVoucher n =
  Voucher
    <$> arbitraryReducedMaybe n -- voucherCategoryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherContactName :: Maybe Text
    <*> arbitrary -- voucherCurrency :: Text
    <*> arbitraryReducedMaybe n -- voucherDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherFileAttachments :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- voucherLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- voucherMetadata :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- voucherNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherOpenAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherPaidDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- voucherPaymentStatus :: Maybe PaymentStatus
    <*> arbitraryReducedMaybe n -- voucherTaxAmounts :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- voucherTaxCondition :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherTotalGrossAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherTotalNetAmount :: Maybe Text
    <*> arbitraryReduced n -- voucherVoucherDate :: Date
    <*> arbitraryReducedMaybe n -- voucherVoucherNumber :: Maybe Text
    <*> arbitraryReduced n -- voucherVoucherStatus :: VoucherStatus
    <*> arbitraryReduced n -- voucherVoucherType :: VoucherType
  
instance Arbitrary VoucherCreate where
  arbitrary = sized genVoucherCreate

genVoucherCreate :: Int -> Gen VoucherCreate
genVoucherCreate n =
  VoucherCreate
    <$> arbitraryReducedMaybe n -- voucherCreateCategoryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherCreateContactId :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherCreateContactName :: Maybe Text
    <*> arbitrary -- voucherCreateCurrency :: Text
    <*> arbitraryReducedMaybe n -- voucherCreateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherCreateFileAttachments :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- voucherCreateLineItems :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- voucherCreateMetadata :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- voucherCreateNotes :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherCreateOpenAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherCreatePaidDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- voucherCreatePaymentStatus :: Maybe PaymentStatus
    <*> arbitraryReducedMaybe n -- voucherCreateTaxAmounts :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- voucherCreateTaxCondition :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherCreateTotalGrossAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- voucherCreateTotalNetAmount :: Maybe Text
    <*> arbitraryReduced n -- voucherCreateVoucherDate :: Date
    <*> arbitraryReducedMaybe n -- voucherCreateVoucherNumber :: Maybe Text
    <*> arbitraryReduced n -- voucherCreateVoucherStatus :: VoucherStatus
    <*> arbitraryReduced n -- voucherCreateVoucherType :: VoucherType
  
instance Arbitrary Warehouse where
  arbitrary = sized genWarehouse

genWarehouse :: Int -> Gen Warehouse
genWarehouse n =
  Warehouse
    <$> arbitraryReducedMaybe n -- warehouseAddressCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseAddressCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- warehouseAddressStreet :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseAddressZip :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseBinLocations :: Maybe AnyType
    <*> arbitrary -- warehouseCode :: Text
    <*> arbitraryReducedMaybe n -- warehouseIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- warehouseIsDefault :: Maybe Bool
    <*> arbitrary -- warehouseName :: Text
    <*> arbitraryReducedMaybe n -- warehouseNotes :: Maybe Text
  
instance Arbitrary WarehouseCreate where
  arbitrary = sized genWarehouseCreate

genWarehouseCreate :: Int -> Gen WarehouseCreate
genWarehouseCreate n =
  WarehouseCreate
    <$> arbitraryReducedMaybe n -- warehouseCreateAddressCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseCreateAddressCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- warehouseCreateAddressStreet :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseCreateAddressZip :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseCreateBinLocations :: Maybe AnyType
    <*> arbitrary -- warehouseCreateCode :: Text
    <*> arbitraryReducedMaybe n -- warehouseCreateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- warehouseCreateIsDefault :: Maybe Bool
    <*> arbitrary -- warehouseCreateName :: Text
    <*> arbitraryReducedMaybe n -- warehouseCreateNotes :: Maybe Text
  
instance Arbitrary WarehouseStock where
  arbitrary = sized genWarehouseStock

genWarehouseStock :: Int -> Gen WarehouseStock
genWarehouseStock n =
  WarehouseStock
    <$> arbitraryReducedMaybe n -- warehouseStockBatchNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseStockBinLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseStockExpiryDate :: Maybe Date
    <*> arbitrary -- warehouseStockProductId :: Text
    <*> arbitrary -- warehouseStockQuantity :: Integer
    <*> arbitraryReducedMaybe n -- warehouseStockSerialNumbers :: Maybe AnyType
    <*> arbitrary -- warehouseStockWarehouseId :: Text
  
instance Arbitrary WarehouseUpdate where
  arbitrary = sized genWarehouseUpdate

genWarehouseUpdate :: Int -> Gen WarehouseUpdate
genWarehouseUpdate n =
  WarehouseUpdate
    <$> arbitraryReducedMaybe n -- warehouseUpdateAddressCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseUpdateAddressCountry :: Maybe CountryCode
    <*> arbitraryReducedMaybe n -- warehouseUpdateAddressStreet :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseUpdateAddressZip :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseUpdateBinLocations :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- warehouseUpdateCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseUpdateIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- warehouseUpdateIsDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- warehouseUpdateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- warehouseUpdateNotes :: Maybe Text
  
instance Arbitrary WebhookEvent where
  arbitrary = sized genWebhookEvent

genWebhookEvent :: Int -> Gen WebhookEvent
genWebhookEvent n =
  WebhookEvent
    <$> arbitraryReducedMaybe n -- webhookEventAttempts :: Maybe Int
    <*> arbitraryReducedMaybe n -- webhookEventChannel :: Maybe Text
    <*> arbitraryReduced n -- webhookEventDirection :: WebhookDirection
    <*> arbitrary -- webhookEventEventType :: Text
    <*> arbitraryReducedMaybe n -- webhookEventLastError :: Maybe Text
    <*> arbitraryReducedMaybe n -- webhookEventPayload :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- webhookEventStatus :: Maybe WebhookEventStatus
  
instance Arbitrary WebhookSubscription where
  arbitrary = sized genWebhookSubscription

genWebhookSubscription :: Int -> Gen WebhookSubscription
genWebhookSubscription n =
  WebhookSubscription
    <$> arbitrary -- webhookSubscriptionEventType :: Text
    <*> arbitraryReducedMaybe n -- webhookSubscriptionIsActive :: Maybe Bool
    <*> arbitrary -- webhookSubscriptionName :: Text
    <*> arbitrary -- webhookSubscriptionSecret :: Text
    <*> arbitrary -- webhookSubscriptionUrl :: Text
  
instance Arbitrary Workflow where
  arbitrary = sized genWorkflow

genWorkflow :: Int -> Gen Workflow
genWorkflow n =
  Workflow
    <$> arbitraryReducedMaybe n -- workflowActions :: Maybe AnyType
    <*> arbitraryReducedMaybe n -- workflowEnabled :: Maybe Bool
    <*> arbitrary -- workflowName :: Text
    <*> arbitrary -- workflowTriggerEvent :: Text
  
instance Arbitrary WorkflowAction where
  arbitrary = sized genWorkflowAction

genWorkflowAction :: Int -> Gen WorkflowAction
genWorkflowAction n =
  WorkflowAction
    <$> arbitrary -- workflowActionActionType :: Text
    <*> arbitraryReducedMaybe n -- workflowActionBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- workflowActionSubject :: Maybe Text
  
instance Arbitrary WorkflowEnabledUpdate where
  arbitrary = sized genWorkflowEnabledUpdate

genWorkflowEnabledUpdate :: Int -> Gen WorkflowEnabledUpdate
genWorkflowEnabledUpdate n =
  WorkflowEnabledUpdate
    <$> arbitrary -- workflowEnabledUpdateEnabled :: Bool
  
instance Arbitrary XRechnungResponse where
  arbitrary = sized genXRechnungResponse

genXRechnungResponse :: Int -> Gen XRechnungResponse
genXRechnungResponse n =
  XRechnungResponse
    <$> arbitrary -- xRechnungResponseContent :: Text
    <*> arbitrary -- xRechnungResponseContentType :: Text
    <*> arbitrary -- xRechnungResponseFilename :: Text
  
instance Arbitrary YearTotal where
  arbitrary = sized genYearTotal

genYearTotal :: Int -> Gen YearTotal
genYearTotal n =
  YearTotal
    <$> arbitrary -- yearTotalTco2e :: Text
    <*> arbitrary -- yearTotalYear :: Int
  
instance Arbitrary YearlyPayrollSummary where
  arbitrary = sized genYearlyPayrollSummary

genYearlyPayrollSummary :: Int -> Gen YearlyPayrollSummary
genYearlyPayrollSummary n =
  YearlyPayrollSummary
    <$> arbitrary -- yearlyPayrollSummaryAvgEmployeeCount :: Int
    <*> arbitraryReduced n -- yearlyPayrollSummaryMonths :: [PayrollSummaryItem]
    <*> arbitrary -- yearlyPayrollSummaryYear :: Int
    <*> arbitrary -- yearlyPayrollSummaryYearlyEmployerCost :: Text
    <*> arbitrary -- yearlyPayrollSummaryYearlyGross :: Text
    <*> arbitrary -- yearlyPayrollSummaryYearlyNet :: Text
  



instance Arbitrary AbsenceStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AbsenceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ActivityStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ActivityType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ApplicationStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AssignmentStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BomStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CheckStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CommunicationChannel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CommunicationDirection where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CompanyType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ConnectorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ContactType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CountryCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CurrencyCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DeclarationType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DeliveryAppointmentStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DeliveryDateStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DiscountType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DocumentType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type10 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type5 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type6 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type7 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type8 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type9 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EmailTemplateStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EmissionMethod where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EmissionTargetScope where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EmployeeStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EmploymentType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ExecutionStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary GatewayType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Gender where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary GhgScope where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InstituteType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InstrumentType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InventoryCountStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InvoiceStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InvoiceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary JobPostingStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary JobStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LanguageCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LeadStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LegalDocType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MessageDirection where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MessageType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MovementType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OrderStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PaymentMethod where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PaymentStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PayrollRunStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PosRegisterStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PosTableStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PostingCategoryType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PrecedingSalesVoucherType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ProductionOrderStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ProformaInvoiceStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PurchaseOrderStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RecurringTemplateType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ReferenceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ReminderLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ReturnOrderStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RfqStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SepaSequenceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ServiceAssignmentStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ServiceJobStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Severity where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SmtpEncryption where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary StockTransferStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SupplierInvoiceStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SupportChannelType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SupportTicketStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SyncLogStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SyncStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SyncType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TicketPriority where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TrainingSource where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary VoucherStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary VoucherType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary WebhookDirection where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary WebhookEventStatus where
  arbitrary = arbitraryBoundedEnum

