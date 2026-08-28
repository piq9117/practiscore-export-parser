{-# LANGUAGE DuplicateRecordFields #-}

module Practiscore.Generator.USPSA (runPsGenCli) where

import Conduit (MonadUnliftIO, runConduitRes, sinkFile, yield, (.|))
import Data.Time.Format (defaultTimeLocale, formatTime)
import Faker (generateNonDeterministic)
import Faker.Company qualified
import Faker.DateTime qualified
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
import Practiscore.USPSA.Parser.Report (Report (..))
import Practiscore.USPSA.Parser.Score (Score (..))
import Practiscore.USPSA.Parser.Shooter (Shooter (..))
import Prettyprinter (Doc, comma, defaultLayoutOptions, layoutPretty, pretty, vsep)
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

prettyReport :: Report -> Doc ann
prettyReport report =
  (pretty $ "A " <> report.summary)
    <> "\n"
    <> pretty shooterHeader
    <> "\n"
    <> (vsep $ (\shooter -> "E " <> prettyShooter shooter) <$> report.shooters)
    <> "\n"
    <> pretty scoreHeader
    <> "\n"
    <> (vsep $ (\score -> "I " <> prettyScore score) <$> report.scores)
    <> "\n"
    <> "$END"

genReport :: (MonadIO m) => Text -> m Report
genReport uspsaMemberId = do
  clubName <- liftIO $ generateNonDeterministic Faker.Company.name
  matchName <- liftIO $ generateNonDeterministic Faker.Company.name
  matchDate <-
    liftIO $
      generateNonDeterministic
        (Faker.DateTime.dayBetweenYears 2020 2026)
  (shooter, scores) <-
    genShooter (UspsaMemberId {unUspsaMemberId = uspsaMemberId}) >>= \shooter -> do
      scores <- replicateM 10 (genScore shooter)
      pure (shooter, scores)
  pure $
    Report
      { summary =
          clubName
            <> ","
            <> matchName
            <> ","
            <> (toText $ formatTime defaultTimeLocale "%m/%d/%y" matchDate),
        shooters = [shooter],
        scores = join scores,
        infoMetadata = []
      }

genCompId :: (MonadIO m) => m CompId
genCompId = do
  unCompId <- randomIO
  pure $
    CompId
      { unCompId
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
        division = "",
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

genScore :: (MonadIO m) => Shooter -> m [Score]
genScore shooter = replicateM 5 $ do
  a <- randomIO @Word8
  b <- randomIO @Word8
  c <- randomIO @Word8
  d <- randomIO @Word8
  time <- randomIO @Double
  rawPoints <- randomIO @Word8
  totalPoints <- randomIO @Word8
  hitFactor <- randomIO @Double
  stagePoints <- randomIO @Double
  stagePlace <- randomIO @Word8
  pure $
    Score
      { gun = "Pistol",
        stage = 0,
        comp = shooter.comp,
        dQ = "",
        dNF = "",
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

shooterHeader :: Text
shooterHeader = "D Comp,USPSA,FirstName,LastName,DQPistol,DQRifle,DQShotgun,Reentry,Class,Division,Match Points,Place Overall,Power Factor,Shotgun Division,Shotgun Power Factor,Shotgun Place Overall,Shotgun Entered,Shotgun Match Points,Rifle Division,Rifle Power Factor,Rifle Place Overall,Rifle Entered,Rifle Match Points,Aggregate,Aggregate Division,Aggregate Pistol Percent,Aggregate Pistol Points,Aggregate Place,Aggregate Rifle Percent,Aggregate Rifle Points,Aggregate Shotgun Percent,Aggregate Shotgun Points,Aggregate Total,Female,Age,Law,Military"

scoreHeader :: Text
scoreHeader = "H Gun,Stage,Comp,DQ,DNF,A,B,C,D,Miss,No Shoot,Procedural,Double Poppers,Double Popper Miss,Late Shot,Extra Shot,Extra Hit,No Penalty Miss,Additional Penalty,Total Penalty,T1,T2,T3,T4,T5,Time,Raw Points,Total Points,Hit Factor,Stage Points,Stage Place,Stage Power Factor"
