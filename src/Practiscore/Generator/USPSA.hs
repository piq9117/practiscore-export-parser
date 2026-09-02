{-# LANGUAGE DuplicateRecordFields #-}

-- module Practiscore.Generator.USPSA (runPsGenCli, prettyReportFields) where
module Practiscore.Generator.USPSA where

import Conduit (MonadUnliftIO, runConduitRes, sinkFile, yield, (.|))
import Data.Time.Format (defaultTimeLocale, formatTime)
import Faker (generateNonDeterministic)
import Faker.Company qualified
import Faker.DateTime qualified
import Faker.DrivingLicense qualified
import Faker.Movie qualified
import Faker.Name qualified
import Options.Applicative
  ( Parser,
    ParserInfo,
    customExecParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    prefs,
    showHelpOnError,
    strOption,
  )
import Practiscore.USPSA (CompId (..), UspsaMemberId (..))
import Practiscore.USPSA.Parser.Report (Report (..), ReportFields (..))
import Practiscore.USPSA.Parser.Score (Score (..))
import Practiscore.USPSA.Parser.Shooter (Shooter (..))
import Practiscore.USPSA.Parser.Stage (StageInfo (..))
import Prettyprinter
  ( Doc,
    comma,
    concatWith,
    defaultLayoutOptions,
    dquotes,
    layoutPretty,
    line,
    pretty,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Text (renderStrict)
import System.Random (randomIO, randomRIO)

data CLI = CLI
  { memberId :: Text,
    filepath :: FilePath
  }
  deriving (Eq, Show)

cli :: Parser CLI
cli =
  CLI
    <$> strOption
      (long "member-id" <> metavar "MEMBER-ID" <> help "MEMBER ID")
    <*> strOption (long "filepath" <> metavar "FILEPATH" <> help "Filepath output")

runPsGenCli :: (MonadUnliftIO m) => m ()
runPsGenCli = do
  cliOutput <- liftIO $ showHelpOnErrorOnExecParser (info (helper <*> cli) fullDesc)
  report <- prettyReport <$> genReport cliOutput.memberId
  runConduitRes $
    yield (encodeUtf8 $ (renderStrict <<< layoutPretty defaultLayoutOptions) report)
      .| sinkFile cliOutput.filepath

showHelpOnErrorOnExecParser :: ParserInfo a -> IO a
showHelpOnErrorOnExecParser = customExecParser (prefs showHelpOnError)

prettyReport :: (Report, [StageInfo]) -> Doc ann
prettyReport (report, stageInfo) =
  (concatWith (\a b -> a <> line <> b <> line) $ (prettyReportFields <<< InfoMetadata) <$> report.infoMetadata)
    <> ("A" <+> pretty report.summary)
    <> line
    <> pretty shooterHeader
    <> line
    <> (vsep $ (\shooter -> "E" <+> prettyShooter shooter) <$> report.shooters)
    <> line
    <> pretty stageHeader
    <> line
    <> (vsep $ (\stage -> "G" <+> prettyStage stage) <$> stageInfo)
    <> line
    <> pretty scoreHeader
    <> line
    <> (vsep $ (\score -> "I" <+> prettyScore score) <$> report.scores)
    <> line
    <> "$END"

genReport :: (MonadIO m) => Text -> m (Report, [StageInfo])
genReport uspsaMemberId = do
  clubName <- liftIO $ generateNonDeterministic Faker.Company.name
  matchName <- liftIO $ generateNonDeterministic Faker.Company.name
  matchDate <-
    liftIO $
      generateNonDeterministic
        (Faker.DateTime.dayBetweenYears 2020 2026)
  shooter <- genShooter (UspsaMemberId {unUspsaMemberId = uspsaMemberId})

  let iteration = take 10 $ iterate (\n -> n + 1) 1
  stages <- forM iteration $ \i ->
    genStage i
  scores <- forM stages $ \stage ->
    genScore stage.number shooter

  let formattedMatchDate = toText $ formatTime defaultTimeLocale "%m/%d/%Y" matchDate

  pure $
    ( Report
        { summary =
            clubName
              <> ","
              <> matchName
              <> ","
              <> formattedMatchDate,
          shooters = [shooter],
          scores = scores,
          infoMetadata =
            [ "Match name: " <> matchName,
              "Match date: " <> formattedMatchDate
            ]
        },
      stages
    )

genCompId :: (MonadIO m) => m CompId
genCompId = do
  unCompId <- randomIO
  pure $
    CompId
      { unCompId
      }

genStage :: (MonadIO m) => Word8 -> m StageInfo
genStage stageNumber = do
  name <- liftIO $ generateNonDeterministic Faker.Movie.title
  isClassifier <- randomRIO (True, False)
  minimumRounds <- randomRIO (5, 32)
  classifierNumber <-
    if isClassifier
      then
        fmap Just $
          liftIO $
            generateNonDeterministic Faker.DrivingLicense.usaPennsylvania
      else pure Nothing
  pure
    StageInfo
      { number = stageNumber,
        gunType = "Pistol",
        minimumRounds,
        maximumPoints = minimumRounds * 5,
        classifier = isClassifier,
        classifierNumber,
        name,
        scoringType = "Comstock",
        timesRun = 1
      }

genShooter :: (MonadIO m) => UspsaMemberId -> m Shooter
genShooter uspsaMemberId = do
  firstname <- liftIO $ generateNonDeterministic Faker.Name.firstName
  lastname <- liftIO $ generateNonDeterministic Faker.Name.lastName
  compId <- genCompId
  class_ <- fmap (\c -> toText [c]) $ randomRIO ('A', 'D')
  matchPoints <- show <$> randomIO @Word8
  placeOverall <- show <$> randomIO @Word8
  pure $
    Shooter
      { comp = Just compId,
        uspsa = Just uspsaMemberId,
        firstname,
        lastname,
        dqpistol = mempty,
        dqrifle = mempty,
        dqshotgun = mempty,
        reentry = mempty,
        class_,
        division = "CO",
        matchPoints,
        placeOverall,
        powerFactor = "125",
        shotgunDivision = "",
        shotgunPowerFactor = "",
        shotgunPlaceOverall = "",
        shotgunEntered = "",
        shotgunMatchPoints = "",
        rifleDivision = "",
        riflePowerFactor = "",
        riflePlaceOverall = "",
        rifleEntered = "",
        rifleMatchPoints = "",
        aggregate = "",
        aggregateDivision = "",
        aggregatePistolPercent = "",
        aggregatePistolPoints = "",
        aggregatePlace = "",
        aggregateRiflePercent = "",
        aggregateRiflePoints = "",
        aggregateShotgunPercent = "",
        aggregateShotgunPoints = "",
        aggregateTotal = "",
        female = "",
        age = "",
        law = "",
        military = ""
      }

genScore :: (MonadIO m) => Word8 -> Shooter -> m Score
genScore stageNumber shooter = do
  a <- randomIO @Word8
  b <- randomIO @Word8
  c <- randomIO @Word8
  d <- randomIO @Word8
  time <- randomRIO (5.0, 35.0)
  rawPoints <- randomRIO (0, 160)
  totalPoints <- randomRIO (0, 160)
  hitFactor <- randomRIO (0.0, 11.0)
  stagePoints <- randomRIO (0.0, 100.0)
  stagePlace <- randomRIO (1, 100)
  pure $
    Score
      { gun = "Pistol",
        stage = stageNumber,
        comp = shooter.comp,
        dQ = "No",
        dNF = "No",
        a,
        b,
        c,
        d,
        miss = 0,
        noShoot = 0,
        procedural = 0,
        doublePoppers = 0,
        doublePopperMiss = 0,
        lateShot = 0,
        extraShot = 0,
        extraHit = 0,
        noPenaltyMiss = 0,
        additionalPenalty = 0,
        totalPenalty = 0,
        t1 = Nothing,
        t2 = Nothing,
        t3 = Nothing,
        t4 = Nothing,
        t5 = Nothing,
        time = Just time,
        rawPoints = Just rawPoints,
        totalPoints = Just totalPoints,
        hitFactor = Just hitFactor,
        stagePoints = Just stagePoints,
        stagePlace = Just stagePlace,
        stagePowerFactor = Nothing
      }

prettyScore :: Score -> Doc ann
prettyScore score =
  pretty score.gun
    <> comma
    <> pretty score.stage
    <> comma
    <> pretty ((.unCompId) <$> score.comp)
    <> comma
    <> pretty score.dQ
    <> comma
    <> pretty score.dNF
    <> comma
    <> pretty score.a
    <> comma
    <> pretty score.b
    <> comma
    <> pretty score.c
    <> comma
    <> pretty score.d
    <> comma
    <> pretty score.miss
    <> comma
    <> pretty score.noShoot
    <> comma
    <> pretty score.procedural
    <> comma
    <> pretty score.doublePoppers
    <> comma
    <> pretty score.doublePopperMiss
    <> comma
    <> pretty score.lateShot
    <> comma
    <> pretty score.extraShot
    <> comma
    <> pretty score.extraHit
    <> comma
    <> pretty score.noPenaltyMiss
    <> comma
    <> pretty score.additionalPenalty
    <> comma
    <> pretty score.totalPenalty
    <> comma
    <> pretty score.t1
    <> comma
    <> pretty score.t2
    <> comma
    <> pretty score.t3
    <> comma
    <> pretty score.t4
    <> comma
    <> pretty score.t5
    <> comma
    <> pretty score.time
    <> comma
    <> pretty score.rawPoints
    <> comma
    <> pretty score.totalPoints
    <> comma
    <> pretty score.hitFactor
    <> comma
    <> pretty score.stagePoints
    <> comma
    <> pretty score.stagePlace
    <> comma
    <> pretty score.stagePowerFactor

prettyShooter :: Shooter -> Doc ann
prettyShooter shooter =
  pretty ((.unCompId) <$> shooter.comp)
    <> comma
    <> pretty ((.unUspsaMemberId) <$> shooter.uspsa)
    <> comma
    <> pretty shooter.firstname
    <> comma
    <> pretty shooter.lastname
    <> comma
    <> pretty shooter.dqpistol
    <> comma
    <> pretty shooter.dqrifle
    <> comma
    <> pretty shooter.dqshotgun
    <> comma
    <> pretty shooter.reentry
    <> comma
    <> pretty shooter.class_
    <> comma
    <> pretty shooter.division
    <> comma
    <> pretty shooter.matchPoints
    <> comma
    <> pretty shooter.placeOverall
    <> comma
    <> pretty shooter.powerFactor
    <> comma
    <> pretty shooter.shotgunDivision
    <> comma
    <> pretty shooter.shotgunPowerFactor
    <> comma
    <> pretty shooter.shotgunPlaceOverall
    <> comma
    <> pretty shooter.shotgunEntered
    <> comma
    <> pretty shooter.shotgunMatchPoints
    <> comma
    <> pretty shooter.rifleDivision
    <> comma
    <> pretty shooter.riflePowerFactor
    <> comma
    <> pretty shooter.riflePlaceOverall
    <> comma
    <> pretty shooter.rifleEntered
    <> comma
    <> pretty shooter.rifleMatchPoints
    <> comma
    <> pretty shooter.aggregate
    <> comma
    <> pretty shooter.aggregateDivision
    <> comma
    <> pretty shooter.aggregatePistolPercent
    <> comma
    <> pretty shooter.aggregatePistolPoints
    <> comma
    <> pretty shooter.aggregatePlace
    <> comma
    <> pretty shooter.aggregateRiflePercent
    <> comma
    <> pretty shooter.aggregateRiflePoints
    <> comma
    <> pretty shooter.aggregateShotgunPercent
    <> comma
    <> pretty shooter.aggregateShotgunPoints
    <> comma
    <> pretty shooter.aggregateShotgunPoints
    <> comma
    <> pretty shooter.aggregateTotal
    <> comma
    <> pretty shooter.female
    <> comma
    <> pretty shooter.age
    <> comma
    <> pretty shooter.law
    <> comma
    <> pretty shooter.military

prettyReportFields :: ReportFields -> Doc ann
prettyReportFields reportFields =
  case reportFields of
    Title title -> "$PRACTISCORE" <+> pretty title
    InfoMetadata metadata -> "$INFO" <+> pretty metadata
    ZMetadata metadata -> "Z" <+> pretty metadata
    Summary summary -> "A" <+> pretty summary
    ShooterHeaderLine headerline -> "D" <+> concatWith (\h d -> h <> comma <> d) (pretty <$> headerline)
    ShooterLine shooterLine -> "E" <+> concatWith (\h d -> h <> comma <> d) (pretty <$> shooterLine)
    StageHeaderLine headerLine -> "F" <+> concatWith (\h d -> h <> comma <> d) (pretty <$> headerLine)
    StageLine stageLine -> "G" <+> concatWith (\h d -> h <> comma <> d) (pretty <$> stageLine)
    ScoreHeaderLine headerLine -> "H" <+> concatWith (\h d -> h <> comma <> d) (pretty <$> headerLine)
    ScoreLine scores -> "I" <+> concatWith (\h d -> h <> comma <> d) (pretty <$> scores)
    End -> "$END"

prettyStage :: StageInfo -> Doc ann
prettyStage stageInfo =
  pretty stageInfo.number
    <> comma
    <> pretty stageInfo.gunType
    <> comma
    <> pretty stageInfo.minimumRounds
    <> comma
    <> pretty stageInfo.maximumPoints
    <> comma
    <> pretty stageInfo.classifier
    <> comma
    <> pretty stageInfo.classifierNumber
    <> comma
    <> (dquotes $ pretty stageInfo.name)
    <> comma
    <> pretty stageInfo.scoringType
    <> comma
    <> pretty stageInfo.timesRun

shooterHeader :: Text
shooterHeader = "D Comp,USPSA,FirstName,LastName,DQPistol,DQRifle,DQShotgun,Reentry,Class,Division,Match Points,Place Overall,Power Factor,Shotgun Division,Shotgun Power Factor,Shotgun Place Overall,Shotgun Entered,Shotgun Match Points,Rifle Division,Rifle Power Factor,Rifle Place Overall,Rifle Entered,Rifle Match Points,Aggregate,Aggregate Division,Aggregate Pistol Percent,Aggregate Pistol Points,Aggregate Place,Aggregate Rifle Percent,Aggregate Rifle Points,Aggregate Shotgun Percent,Aggregate Shotgun Points,Aggregate Total,Female,Age,Law,Military"

stageHeader :: Text
stageHeader = "F Number,Guntype,Minimum Rounds,Maximum Points,Classifier,Classifier_No,Stage_name,ScoringType,TimesRun"

scoreHeader :: Text
scoreHeader = "H Gun,Stage,Comp,DQ,DNF,A,B,C,D,Miss,No Shoot,Procedural,Double Poppers,Double Popper Miss,Late Shot,Extra Shot,Extra Hit,No Penalty Miss,Additional Penalty,Total Penalty,T1,T2,T3,T4,T5,Time,Raw Points,Total Points,Hit Factor,Stage Points,Stage Place,Stage Power Factor"
