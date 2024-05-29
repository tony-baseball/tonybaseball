# R/tonybaseball_functions.R

#' Convert Latest Boomers game to BATS format
#'
#' This function makes a lot of transformations to the Yakkertech file and transforms it into a standard Trackman format that is compatible with BATS Trackman Merge
#'
#' @param date in a %Y-%m-%d format.
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
db_to_bats <- function(date) {
  query <- paste0("SELECT * FROM yak_24 WHERE Date = :date
                  and (HomeTeamCode = 'BOOM' or AwayTeamCode = 'BOOM')")

  df <- RSQLite::dbGetQuery(db, query, params = list(date = max_date)) %>%
    dplyr::rename(PitchNo = 1) %>%
    dplyr::mutate(Date = as.Date(Date),
                  Batter = stringr::str_to_title(Batter),
                  Pitcher = stringr::str_to_title(Pitcher),
                  Batter = stringr::str_replace_all(Batter, c("DepretaJohnson" = "Depreta-Johnson",
                                                              "Depreta-johnson" = "Depreta-Johnson",
                                                              "Depretajohnson" = "Depreta-Johnson")),
                  Batter = paste0(sub("^[^ ]* ", "", Batter), ", ", sub(" .*", "", Batter)),
                  Pitcher = paste0(sub("^[^ ]* ", "", Pitcher), ", ", sub(" .*", "", Pitcher)),
                  Catcher = paste0(sub("^[^ ]* ", "", Catcher), ", ", sub(" .*", "", Catcher))
    )   %>%
    dplyr:: mutate(BatterTeam = team_info$bats_team_code[match(BatterTeam, team_info$team_FL)],
                   PitcherTeam = team_info$bats_team_code[match(PitcherTeam, team_info$team_FL)],
                   HomeTeam = team_info$bats_team_code[match(HomeTeam, team_info$team_FL)],
                   AwayTeam = team_info$bats_team_code[match(AwayTeam, team_info$team_FL)],
                   CatcherTeam = team_info$bats_team_code[match(CatcherTeam, team_info$team_FL)] ,
                   Time = ifelse(RelSpeed =="", paste(""),
                                 Time),
                   PitchCall = gsub("Foul", "FoulBall", PitchCall),
                   TaggedPitchType = gsub("Changeup", "ChangeUp", TaggedPitchType) )%>%
    dplyr::relocate(Catcher, .after = PitchUUID) %>%
    dplyr::relocate(CatcherId, .after = Catcher) %>%
    dplyr::relocate(CatcherTeam, .after = CatcherId) %>%
    dplyr::select(-c(PitchClass, HitDirection1, HitDirection2, hc_x, hc_y, launch_speed, launch_angle, hardhit, weakhit,
                     whiff, swing, take, in_zone,zone_x, zone_y, filter_col, barrel, HomeTeamCode, AwayTeamCode, Code,
                     yt_RelSpeed, yt_RelHeight, yt_RelSide, yt_VertRelAngle, yt_HorzRelAngle, yt_ZoneSpeed,  yt_ZoneTime, yt_HorzBreak,
                     yt_InducedVertBreak, yt_OutOfPlane, yt_FSRI, yt_EffectiveSpin, yt_GyroSpin, yt_Efficiency, yt_HorzApprAngle, yt_PlateLocHeight,
                     yt_SpinComponentX, yt_SpinComponentY, yt_SpinComponentZ,  yt_HitVelocityX, yt_HitVelocityY, yt_HitVelocityZ, yt_HitLocationX,
                     yt_HitLocationY, yt_HitLocationZ, yt_GroundLocationX, yt_GroundLocationY,  yt_HitBreakX, yt_HitBreakY, yt_HitBreakT,
                     yt_HitSpinComponentX,  yt_HitSpinComponentY, yt_HitSpinComponentZ, yt_SessionName,  yt_PitchSpinConfidence, yt_PitchReleaseConfidence,
                     yt_HitSpinConfidence, yt_EffectiveBattingSpeed, yt_ReleaseAccuracy, yt_ZoneAccuracy, yt_SeamLat, yt_SeamLong, yt_ReleaseDistance,
                     yt_AeroModel, yt_PlateLocSide, yt_VertApprAngle, Umpire, SEASON, xBA, x1B,
                     x2B, x3B, xHR, xOut, xSLG, woba_weight, xwOBACON, Note
    ))

  return(df)
}

#' Convert Latest Boomers game to BATS format
#'
#' This function makes a lot of transformations to the Yakkertech file and transforms it into a standard Trackman format that is compatible with BATS Trackman Merge
#'
#' @param date in a %Y-%m-%d format.
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
db_to_bats <- function(date) {
  query <- paste0("SELECT * FROM yak_24 WHERE Date = :date
                  and (HomeTeamCode = 'BOOM' or AwayTeamCode = 'BOOM')")

  df <- RSQLite::dbGetQuery(db, query, params = list(date = max_date)) %>%
    dplyr::rename(PitchNo = 1) %>%
    dplyr::mutate(Date = as.Date(Date),
                  Batter = stringr::str_to_title(Batter),
                  Pitcher = stringr::str_to_title(Pitcher),
                  Batter = stringr::str_replace_all(Batter, c("DepretaJohnson" = "Depreta-Johnson",
                                                              "Depreta-johnson" = "Depreta-Johnson",
                                                              "Depretajohnson" = "Depreta-Johnson")),
                  Batter = paste0(sub("^[^ ]* ", "", Batter), ", ", sub(" .*", "", Batter)),
                  Pitcher = paste0(sub("^[^ ]* ", "", Pitcher), ", ", sub(" .*", "", Pitcher)),
                  Catcher = paste0(sub("^[^ ]* ", "", Catcher), ", ", sub(" .*", "", Catcher))
    )   %>%
    dplyr:: mutate(BatterTeam = team_info$bats_team_code[match(BatterTeam, team_info$team_FL)],
                   PitcherTeam = team_info$bats_team_code[match(PitcherTeam, team_info$team_FL)],
                   HomeTeam = team_info$bats_team_code[match(HomeTeam, team_info$team_FL)],
                   AwayTeam = team_info$bats_team_code[match(AwayTeam, team_info$team_FL)],
                   CatcherTeam = team_info$bats_team_code[match(CatcherTeam, team_info$team_FL)] ,
                   Time = ifelse(RelSpeed =="", paste(""),
                                 Time),
                   PitchCall = gsub("Foul", "FoulBall", PitchCall),
                   TaggedPitchType = gsub("Changeup", "ChangeUp", TaggedPitchType) )%>%
    dplyr::relocate(Catcher, .after = PitchUUID) %>%
    dplyr::relocate(CatcherId, .after = Catcher) %>%
    dplyr::relocate(CatcherTeam, .after = CatcherId) %>%
    dplyr::select(-c(PitchClass, HitDirection1, HitDirection2, hc_x, hc_y, launch_speed, launch_angle, hardhit, weakhit,
                     whiff, swing, take, in_zone,zone_x, zone_y, filter_col, barrel, HomeTeamCode, AwayTeamCode, Code,
                     yt_RelSpeed, yt_RelHeight, yt_RelSide, yt_VertRelAngle, yt_HorzRelAngle, yt_ZoneSpeed,  yt_ZoneTime, yt_HorzBreak,
                     yt_InducedVertBreak, yt_OutOfPlane, yt_FSRI, yt_EffectiveSpin, yt_GyroSpin, yt_Efficiency, yt_HorzApprAngle, yt_PlateLocHeight,
                     yt_SpinComponentX, yt_SpinComponentY, yt_SpinComponentZ,  yt_HitVelocityX, yt_HitVelocityY, yt_HitVelocityZ, yt_HitLocationX,
                     yt_HitLocationY, yt_HitLocationZ, yt_GroundLocationX, yt_GroundLocationY,  yt_HitBreakX, yt_HitBreakY, yt_HitBreakT,
                     yt_HitSpinComponentX,  yt_HitSpinComponentY, yt_HitSpinComponentZ, yt_SessionName,  yt_PitchSpinConfidence, yt_PitchReleaseConfidence,
                     yt_HitSpinConfidence, yt_EffectiveBattingSpeed, yt_ReleaseAccuracy, yt_ZoneAccuracy, yt_SeamLat, yt_SeamLong, yt_ReleaseDistance,
                     yt_AeroModel, yt_PlateLocSide, yt_VertApprAngle, Umpire, SEASON, xBA, x1B,
                     x2B, x3B, xHR, xOut, xSLG, woba_weight, xwOBACON, Note
    ))

  return(df)
}



#' Predict AutoTaggedPitchType
#'
#' This function predicts FL pitch types based on baseball savant classifications
#'
#' @param data, require InducedVertBreak, HorzBreak, SpinRate, SpinAxis, and PitchUUID to NOT be NA
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
predict_pitch_type <- function(data) {

  lh_fb <- readRDS(file = "C:/Users/tdmed/OneDrive/R_Codes/MLB_Modeling/knnModel_LHFB.rds")
  rh_fb <- readRDS(file = "C:/Users/tdmed/OneDrive/R_Codes/MLB_Modeling/knnModel_RHFB.rds")
  lh_bb <- readRDS(file = "C:/Users/tdmed/OneDrive/R_Codes/MLB_Modeling/knnModel_LHBB.rds")
  rh_bb <- readRDS(file = "C:/Users/tdmed/OneDrive/R_Codes/MLB_Modeling/knnModel_RHBB.rds")
  # input

  filtered_data <- data %>%
    dplyr::filter(!is.na(InducedVertBreak) &
                    !is.na(HorzBreak) &
                    !is.na(SpinRate) &
                    !is.na(SpinAxis)
    ) %>%
    dplyr::mutate(AutoPitchType = case_when(
      PitcherThrows == "Right" & TaggedPitchType %in% c("Fastball", "Sinker") ~ predict(rh_fb, .),
      PitcherThrows == "Left" & TaggedPitchType %in% c("Fastball", "Sinker") ~ predict(lh_fb, .),
      PitcherThrows == "Right" & TaggedPitchType %in% c("Curveball", "Slider", "Cutter")  ~ predict(rh_bb, .),
      PitcherThrows == "Left" & TaggedPitchType %in% c("Curveball", "Slider", "Cutter")  ~ predict(lh_bb, .),
      TRUE ~ TaggedPitchType
    ))

  data$AutoPitchType <- filtered_data$AutoPitchType[match(data$PitchUUID, filtered_data$PitchUUID)]
  data$AutoPitchType <- ifelse(is.na(data$AutoPitchType), data$TaggedPitchType, data$AutoPitchType)
  return(data)
}


#' Clean Team Names across BAtterTeam,PitcherTeam,CatcherTeam,HomeTeam,AwayTeam
#'
#' @param data,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
yt_clean_team_names <- function(data) {
  data <- data %>%
    dplyr::mutate(across(c(HomeTeam,AwayTeam, BatterTeam, PitcherTeam, CatcherTeam),
                         ~ case_when(
                           grepl("yalls|Yalls|Y'alls", .) ~ "Florence Y'alls",
                           grepl('quebec|Quebec', .) ~ "Quebec Capitales",
                           grepl('jackals|Jackals', .) ~ "New Jersey Jackals",
                           grepl('boulders|Boulders', .) ~ "New York Boulders",
                           grepl('Thunder|thunder', .) ~ "Windy City Thunderbolts",
                           grepl('vansville|vensville', .) ~ "Evansville Otters",
                           grepl('Schaumburg|Schamburg|oomer', .) ~ "Schaumburg Boomers",
                           grepl('Trois|trois|iviere|igles', .) ~ "Trois-Rivieres Aigles",
                           grepl('alley|TCVC|TVCV|Tcvc|Tcvc 24|Tcvc 24', .) ~ "Tri-City ValleyCats",
                           grepl('oliet|lammer', .) ~ "Joliet Slammers",
                           grepl('erie|rusher', .) ~ "Lake Erie Crushers",
                           grepl('ateway|rizzlie', .) ~ "Gateway Grizzlies",
                           grepl('ttawa|itans', .) ~ "Ottawa Titans",
                           grepl('ashington|Wild', .) ~ "Washington Wild Things",
                           grepl('ussex|iners', .) ~ "Sussex County Miners",
                           grepl('ngland|nockout', .) ~ "New England Knockouts",
                           T ~ .
                         )
    ),
    across(c(HomeTeam,AwayTeam, BatterTeam, PitcherTeam, CatcherTeam),
           ~ case_when(
             grepl('alley|TCVC|TVCV|Tcvc|Tcvc 24|Tcvc 24', .) ~ "Tri-City ValleyCats",
             T~ .

           )
    )
    )
  return(data)
}


#' model xStats on a yakkertech/trackman file
#'
#' @param data,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export

# usethis::use_data(model_1B, model_2B, model_3B, model_HR, xBA_model, internal = T, overwrite = T)
# model_1B <- readRDS(system.file("extdata", "model_1B.rds", package = "tonybaseball"))
# model_2B <- readRDS(system.file("extdata", "model_2B.rds", package = "tonybaseball"))
# model_3B <- readRDS(system.file("extdata", "model_3B.rds", package = "tonybaseball"))
# model_HR <- readRDS(system.file("extdata", "model_HR.rds", package = "tonybaseball"))
# xBA_model <- readRDS(system.file("extdata", "xBA_model.rds", package = "tonybaseball"))

xStats_yak <- function(data) {


  weights <- dbGetQuery(db, "SELECT * FROM weights")

  data <- data %>%
    mutate(
      xBA = ifelse(
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle), round(predict(xBA_model, newdata = data, type = "response"),6),
        NA),
      x1B = ifelse(
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle), round(predict(model_1B, newdata = data, type = "response"),6),
        NA),
      x2B = ifelse(
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle), round(predict(model_2B, newdata = data, type = "response"),6),
        NA),
      x3B = ifelse(
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle), round(predict(model_3B, newdata = data, type = "response"),6),
        NA),
      xHR = ifelse(
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle), round(predict(model_HR, newdata = data, type = "response"),6),
        NA),
      xOut = 1 - (x1B + x2B + x3B + xHR),
      xSLG = round(x1B + (x2B * 2) + (x3B * 3) + (xHR * 4),6),
      woba_weight = case_when(
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle) & PlayResult == 'Single' ~ weights$w1B,
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle) & PlayResult == 'Double' ~ weights$w2B,
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle) & PlayResult == 'Triple' ~ weights$w3B,
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle) & PlayResult == 'HomeRun' ~ weights$wHR,
        PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle) & PlayResult == '' ~ NA,
        T ~ 0
      ),
      xwOBACON = (x1B * weights$w1B) + (x2B * weights$w2B) + (x3B * weights$w3B) + (xHR * weights$wHR),
      sweetspot = case_when(
        PitchCall == 'InPlay' & Angle > 40 ~ 'Under',
        PitchCall == 'InPlay' & between(Angle,32,40) ~ 'Flare',
        PitchCall == 'InPlay' & between(Angle,8,32) ~ 'SweetSpot',
        PitchCall == 'InPlay' & between(Angle,0,8) ~ 'Burner',
        PitchCall == 'InPlay' & Angle < 0 ~ 'Topped',
        T ~ NA),
      bbe = ifelse(PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle), 1, 0)

    )


  return(data)
}



#' transform yt file for db use
#'
#' @param data,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
yt_transform_file <- function(data) {

  data <- data  %>%
    dplyr::mutate(# Date = gsub('0024-','2024-', Date)),
      Date = as.Date(Date, '%m/%d/%Y'),
      PitchCall = gsub("FoulTip",'StrikeSwinging', PitchCall),
      HitType =gsub('Popup','PopUp', HitType),
      HitType = gsub('Flyball', 'FlyBall', HitType),
      HitType = gsub('Groundball', 'GroundBall', HitType),
      HitType = ifelse(HitType == ""  & Angle < 10, 'GroundBall',
                       ifelse(HitType == "" & Angle >=10 & Angle <= 25, 'LineDrive',
                              ifelse(HitType == "" & Angle > 25 & Angle < 50, 'FlyBall',
                                     ifelse(HitType == "" & Angle > 50,  'PopUp', HitType )) ) ),
      PitchClass = ifelse(TaggedPitchType == 'Fastball' | TaggedPitchType == 'Sinker' | TaggedPitchType == 'Cutter', 'Fastballs',
                          ifelse(TaggedPitchType == 'Curveball' | TaggedPitchType == 'Slider', 'BreakingBalls',
                                 'Offspeed'    ))  ,
      PitchClass = recode(PitchClass, Fastballs = 'Fastballs', BreakingBalls = 'BreakingBalls', Offspeed = 'Offspeed'),
      HitDirection1 = ifelse(Direction > -15 & Direction < 15, 'Center',
                             ifelse(Direction >= 15, 'Right',
                                    'Left' )),
      HitDirection2 = ifelse(Direction > -9 & Direction < 9, 'Center',
                             ifelse(Direction >= 9 & Direction < 27, 'RightCenter',
                                    ifelse(Direction >= 27, 'Right',
                                           ifelse(Direction < -9 & Direction >=-27, 'LeftCenter',
                                                  'Left' ) ) )),
      hc_x = sin(Bearing * pi/180)*Distance ,
      hc_y = cos(Bearing * pi/180)*Distance,
      BatterTeam = stringr::str_to_title(BatterTeam),
      Batter = stringr::str_to_title(Batter),
      BatterTeam = stringr::str_squish(BatterTeam),
      Batter = stringr::str_squish(Batter),
      PitcherTeam = stringr::str_to_title(PitcherTeam),
      Pitcher = stringr::str_to_title(Pitcher),
      PitcherTeam = stringr::str_squish(PitcherTeam),
      Pitcher = stringr::str_squish(Pitcher),
      launch_speed = round(ExitSpeed),
      launch_angle = round(Angle),
      hardhit = ifelse(ExitSpeed >= 95 & PitchCall == 'InPlay', 1, 0),
      weakhit = ifelse(ExitSpeed < 80 & PitchCall == 'InPlay', 1, 0),
      whiff = ifelse(PitchCall == 'StrikeSwinging', 1,0),
      swing = ifelse(PitchCall %in% c('StrikeSwinging', 'InPlay', 'FoulBall', 'Foul', 'FoulTip'),1,0),
      take = ifelse(PitchCall %in% c('StrikeCalled','BallCalled'),1,0),
      in_zone = ifelse(between(PlateLocSide, -.83, .83) & between(PlateLocHeight,1.375,3.625),1,0 ),.after = RunsScored ) %>%
    # ZONE LOCATIONS
    dplyr::mutate(zone_x = ifelse(PlateLocSide >= -.83 & PlateLocSide < -.2765, -.55,
                                  ifelse(PlateLocSide >= -.2765 & PlateLocSide < .2765, 0,
                                         ifelse(PlateLocSide >= .2765 & PlateLocSide <= .83,.55,2 ) ) ),
                  zone_y =ifelse (PlateLocHeight < 3.5 & PlateLocHeight >2.83, 3.17,
                                  ifelse(PlateLocHeight < 2.83 & PlateLocHeight >= 2.16, 2.5,
                                         ifelse(PlateLocHeight < 2.16 & PlateLocHeight >= 1.77, 1.82,2 ) ) ),
                  filter_col = ifelse(whiff == 1, 'Whiff',
                                      # ifelse(hardhit == 1, 'HardHit',
                                      #        ifelse(weakhit == 1, 'WeakHit',
                                      'All'
                                      # ifelse(swing == 1, 'Swing',
                                      #        ifelse(swing == 0, 'Take',''
                                      #)  )
                  )#))
                  , .after = PlateLocSide) %>%
    baseballr::code_barrel() %>%
    arrange(desc(Date), GameID, Time)  %>%
    # FIX SWITCH HITTERS
    mutate(BatterSide = ifelse( BatterSide == "Switch" & PitcherThrows == 'Left', 'Right',
                                ifelse(BatterSide == "Switch" & PitcherThrows == 'Right', 'Left', BatterSide) )) %>%
    # FIX PLAYER NAMES
    mutate(
      across(c(Batter, Pitcher, Catcher, PitcherId), ~ stringr::str_trim(stringr::str_squish(stringr::str_to_title(.)))),
      across(c(Batter, Pitcher, Catcher, PitcherId),
             ~stringr::str_replace_all(
               .,
               c('Drew  Stengren' = 'Drew Stengren',
                 'Tyler  Osik' = 'Tyler Osik',
                 'Von zboray' = 'Von Zboray',
                 'Cal Mcannich'  = 'Cal Mcaninch',
                 'Cal Macaninch' = 'Cal Mcaninch',
                 'Cal Mcaninch'  = 'Cal Mcaninch',
                 'C\\.mcannich'  = 'Cal Mcaninch',
                 'C\\.Mcannich'  = 'Cal Mcaninch',
                 'Cal Mcanich' = 'Cal Mcaninch',
                 'Gage Howard' = 'Gaige Howard',
                 'D.j. Stewart' = 'D.J. Stewart',
                 'A.j. Wright' = 'A.J. Wright',
                 'Aj Wright' = 'A.J. Wright',
                 'AJ Wright' = 'A.J. Wright',
                 'Peter Zimmerman' = 'Peter Zimmermann',
                 'Joe Deluca' = 'Joe DeLuca',
                 'Matt Mcdermott' = 'Matt McDermott',
                 'Parker Depasquale' = 'Parker DePasquale',
                 'L-P Pelletier' = 'L.P. Pelletier',
                 'L.p. Pelletier' = 'L.P. Pelletier',
                 'Lp Pelletier' = 'L.P. Pelletier',
                 'Louis-Philippe Pelletier' = 'L.P. Pelletier',
                 'Louis-Phillippe Pelletier' = 'L.P. Pelletier',
                 'Jc Santini' = 'J.C. Santini',
                 'J.c. Santini' = 'J.C. Santini',
                 'Jr Disarcina' = 'J.R. DiSarcina',
                 'J.r. Disarcina' = 'J.R. DiSarcina',
                 'Tj White' = 'T.J. White',
                 'T.j. White' = 'T.J. White',
                 'Vince Byrd' = 'Vincent Byrd, Jr.',
                 'Chris Ruiz' = 'Cris Ruiz',
                 'Carson Mccusker' = 'Carson McCusker',
                 'Tyler Depretajohnson' = 'Tyler Depreta-Johnson',
                 'Tyler DepretaJohnson' = 'Tyler Depreta-Johnson',
                 'Tyler Depreta-Johnson' = 'Tyler Depreta-Johnson',
                 'Jp Fullerton' = 'J.P. Fullerton',
                 'Gj Hill' = 'GJ Hill',
                 'G.j. Gill' = 'GJ Gill',
                 'Matt Mcgarry' = 'Matt McGarry',
                 "Ti'quan" = "Ti'Quan",
                 'Paul Coumoulous' = 'Paul Coumoulos',
                 'Liam Mcarthur' = 'Liam McArthur',
                 'Todd Isaacs Jr.' = 'Todd Isaacs',
                 'Mike Howard' = 'Brandon Backman',
                 'Christian Lopez' = 'Cristian Lopez',
                 'Carson Larue' = 'Carson LaRue',
                 'Nick Macdonald' = 'Nick MacDonald',
                 'Mcalister' = 'McAlister',
                 'Jack Mcmahill' = 'Jake McMahill',
                 'Evan Braband' = 'Evan Brabrand',
                 'Cam Pferrer' = 'Cameron Pferrer',
                 'Austin Grossman' = 'Austin Gossmann',
                 'Austin Gossman' = 'Austin Gossmann',
                 'Austin Gossmann' = 'Austin Gossmann',
                 'Austin Gossmannn' = 'Austin Gossmann',
                 'Amaury Telemaco jr' = 'Amaury Telemaco, Jr.',
                 'Jd Hammer' = 'J.D. Hammer',
                 'J\\.d\\. Hammer' = 'J.D. Hammer',
                 'Reymin Guaduan' = 'Reymin Guduan',
                 'Reyman Guaduan' = 'Reymin Guduan',
                 'Dan Bebee' = 'Dan Beebe',
                 'Coleman Huntley Iii' = 'Coleman Huntley',
                 'Rafi Vazquez ' = 'Rafi Vazquez',
                 'Brad Vanasdlen' = 'Brad VanAsdlen',
                 'Cam Aufderheide' = 'Cameron Aufderheide',
                 "Caden O'Brien" = "Caden OBrien",
                 "Caden O'brien" = "Caden OBrien",
                 "Brain Mckenna" = "Brian McKenna",
                 "Ryan O'reilly" = "Ryan O'Reilly",
                 "Ryan Oreilly" = "Ryan O'Reilly",
                 "Mike Desanti" = "Mike DeSanti",
                 "Logan Schmidt" = "Logan Schmitt",
                 'Yuri Sako' = 'Yuhi Sako',
                 'Will Mclean' = 'Will Maclean',
                 'W\\. Lombard' = 'Weston Lombard',
                 'Rob Klinchock' = 'Robert Klinchock',
                 'Nick Ernest' = 'Nick Ernst',
                 'Nathaniel Tate' = 'Nathanial Tate',
                 'Matt Still' = 'Matt Stil' ,
                 'Kenny Willians' = 'Kenny Williams',
                 'Schuefland' = 'Schulefand',
                 'Joe Laforia' = 'Joe Lafiora',
                 'Jason Pinexa' = 'Jason Pineda',
                 'Harry Gollert' = 'Harley Gollert',
                 'Evy Rubial' = 'Evy Ruibal',
                 'Emile Boles' = 'Emile Boies',
                 'Elliot Carnery' = 'Elliot Carney',
                 'E\\.rutckyj' = 'Evan Rutckyj',
                 'Damon Casetta-Stubs' = 'Damon Casetta-Stubbs',
                 'Clarke Davenpoty' = 'Clarke Davenport',
                 'Christian Scafildi' = 'Christian Scafidi',
                 'Caden Obrien' = 'Caden OBrien',
                 'Brandon Backmon' = 'Brandon Backman',
                 'Brendon Backman' = 'Brandon Backman',
                 'Ben Terwillger' = 'Ben Terwilliger',
                 'Aaron Dana' = 'Aaron Dona',
                 'Alexander Ovalles' = 'Alex Ovalles',
                 'Mcneely' = 'McNeely',
                 "Mccarthy" = 'McCarthy',
                 'Dj Stewart' = 'D.J. Stewart',
                 'Edouard Savoie' = 'Eddy Savoie',
                 "Elvis Peralta" = "Elvis Peralta, Jr\\.",
                 'Evan Berkley' = 'Evan Berkey',
                 'Fransisco Hernandez' = 'Francisco Hernandez',
                 'Jake Mackenzie' = 'Jake MacKenzie',
                 'Jj' = 'JJ',
                 'Justin Gideion'= 'Justin Gideon',
                 'Mark Herron Jr' = 'Mark Herron, Jr\\.',
                 'Mark Herron Jr.' = 'Mark Herron, Jr\\.',
                 'Ron Washington Jr.' = 'Ron Washington',
                 'Ronnie Allen Jr.' = 'Ronnie Allen, Jr',
                 'Vincent Byrd Jr.' = 'Vincent Byrd, Jr\\.',
                 "Jose Ledesma Jr." = "Jose Ledesma Jr",
                 'Sergio Gutiererz' = 'Sergio Gutierrez',
                 'Tj Reeves' = 'T.J. Reeves',
                 'Ben Terwillinger' = 'Ben Terwilliger',
                 'Louiber' = 'Loubier',
                 'Marklynd' = 'Marklund',
                 'Christopher Mormile' = 'Chris Mormile',
                 'Mascatiello' = 'Moscatiello',
                 "Guiliano" = 'Giuliano',
                 'Giuiliano' = 'Giuliano',
                 'Gyeogju' = 'Gyeongju',
                 'Woolfork' = 'Woolfolk',
                 'Jalon Long' = 'Jalon Tyson-Long',
                 'Divalerio' = 'DiValerio',
                 'Jordan Divalerio' = 'Jordan DiValerio',
                 'Kent Kylman' = 'Kent Klyman',
                 "O'sullivan" = "O'Sullivan",
                 "Liam O'sullivan" = "Liam O'Sullivan",
                 'Mclain' = 'McLain',
                 'Mclain Harris' = 'McLain Harris',
                 "O'donnell" = "O'Donnell",
                 'Parker Brahams' = 'Parker Brahms',
                 'Ronaldo Heredia-Bustos' = 'Rolando Heredia-Bustos',
                 'Tyler Laporte' = 'Tyler LaPorte',
                 'Will Ambruester' = 'Will Armbruester',
                 'Yusniel Padron' = 'Yusnier Padron',
                 'Anthony Gomez' = 'Tony Gomez',
                 "Nico O'donnell" = "Nico O'Donnell",
                 'Deluca' = 'DeLuca',
                 'N. Gotta' = 'Nick Gotta',
                 'Braedon Allemann' = 'Braeden Allemann'
               )
             )
      ),
      Pitcher = gsub('C.mcaninch','Cal Mcaninch', Pitcher),
      Pitcher = gsub('A.husson', 'Aaron Husson', Pitcher),
      Pitcher = gsub('Austin Gossmannn', 'Austin Gossmann', Pitcher),
      PitcherThrows = ifelse(Pitcher == 'Carlos Sano', 'Right', PitcherThrows),
      Batter = ifelse(grepl("Peter Zimm", Batter), "Peter Zimmermann", Batter),
      PitcherThrows = ifelse(Pitcher == 'Carlos Sano', 'Right', PitcherThrows),
      BatterSide = ifelse(Batter == 'Christian Fedko', 'Right', BatterSide),
      BatterTeam = case_when(
        `Top.Bottom` == 'Top' ~ AwayTeam,
        `Top.Bottom` == 'Bottom' ~ HomeTeam,
        T ~ BatterTeam
      ),
      PitcherTeam = case_when(
        `Top.Bottom` == 'Bottom' ~ AwayTeam,
        `Top.Bottom` == 'Top' ~ HomeTeam,
        T ~ PitcherTeam
      )
    ) %>%
    dplyr::mutate(SpinRate = as.numeric(SpinRate)) %>%
    dplyr::mutate(SEASON = 2024, .after = barrel) %>%
    dplyr::relocate(yt_AeroModel, .after = SEASON)


  return(data)
}


#' add team codes for bats/yakkertech
#'
#' @param data,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
yt_add_team_codes <- function(data) {
  data <- data %>%
    dplyr:: mutate(HomeTeamCode = team_info$bats_team_code[match(data$HomeTeam, team_info$team_FL)],
                   AwayTeamCode = team_info$bats_team_code[match(data$AwayTeam, team_info$team_FL)],
                   Date = ifelse(grepl("/", data$Date),
                                 format(as.Date(Date, format = "%m/%d/%Y"), "%Y-%m-%d"),
                                 format(ymd(Date), "%Y-%m-%d")),
                   Code = n() ) %>%
    arrange(PitchNo)
  return(data)
}


#' add umpire from ump24 table in db
#'
#' @param data,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
yt_add_umpire <- function(data) {

  data$Umpire <- umps$Umpire[match(paste0(data$Date,data$HomeTeam),paste0(umps$Date,umps$HomeTeam))]

  return(data)
}

#' quick pitch characteristics table
#'
#' @param data,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
pitcher_pitch_metrics <- function(data) {

  table <- data %>%
    # using recode will allow us to save space on the document
    dplyr:: mutate(TaggedPitchType = factor(TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "Changeup", "Splitter", 'Knuckleball', 'Other')),
                   TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Knuckleball = 'KN', Other = 'OT' )
                   ) %>%
    group_by(Pitcher,
             'Pitch' = TaggedPitchType) %>%
    dplyr::summarize('No.' = n(),
                     'Usage' = n(),
                     'Usage %' = n(),
                     'Velo' = round(mean(RelSpeed, na.rm = TRUE),1),
                     'VeloMax' = round(max(RelSpeed, na.rm = TRUE),1),
                     'Tilt' = round(mean(SpinAxis, na.rm = TRUE),0),
                     'Time' = sapply(`Tilt`, function(x) if (is.na(x)){return(NA)}
                                     else if(x > 180 & x <= 360){(x/30)-6}
                                     else if(x == 180){12}
                                     else{(x/30)+6}),
                     'HH' = as.integer(Time),
                     'HH' = sapply(HH, function(x) if (is.na(x)){return(NA)}
                                   else if(x == 0){x+12}
                                   else if(x > 12){x-12}
                                   else{x+0}),
                     "MM" = formatC(round((Time%%1)*60, digits = 0), width = 2, flag = "0"),
                     'Tilt' = paste0(HH,":", MM),
                     'Spin' = round(mean(SpinRate, na.rm = TRUE),0),
                     'SpinEff%' = round(mean(yt_Efficiency, na.rm= TRUE),0),
                     'Vert' = round(mean(InducedVertBreak, na.rm = TRUE),1),
                     'Horz' = round(mean(HorzBreak, na.rm = TRUE),1),
                     'VAA' = round(mean(VertApprAngle, na.rm = TRUE),1),
                     'RelHt' = round(mean(RelHeight, na.rm = TRUE),1),
                     'RelSide' = round(mean(RelSide, na.rm = TRUE),1),
                     'Ext' = round(mean(Extension, na.rm = TRUE),1)
    ) %>%
    mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>%
    dplyr::select(-Usage,-Time,-HH,-MM) %>%
    arrange(Pitcher, Pitch)

  return(table)

}
