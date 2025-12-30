# practiscore-export-parser

**This project is not affiliiated with practiscore. This is a personal project 
that parses individual competitor's data from practiscore output**

## ps-tap
`ps-tap` extracts data from the web report export based on member_id and outputs a csv file

```sh
Usage: ps-tap --member-id MEMBER-ID (--uspsa | --scsa) --report REPORT_PATH
              --output FILE TO OUTPUT INDIVIDUAL SHOOTER DATA

Available options:
  -h,--help                Show this help text
  --member-id MEMBER-ID    MEMBER ID
  --uspsa                  Indicate USPSA
  --scsa                   Indicate SCSA
  --report REPORT_PATH     Relative path of the report
  --output FILE TO OUTPUT INDIVIDUAL SHOOTER DATA
                           Relative path to output individual report
```

***Example Output***

USPSA
```csv
uspsa_id,firstname,lastname,class,division,match_points,place_overall,gun,stage,A,B,C,D,miss,no_shoot,procedural,time,raw_points,total_points,hit_factor,stage_place
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,1,24,0,1,0,3,0,0,19.61,123,93,4.7425,11
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,2,15,0,9,0,0,0,0,16.54,102,102,6.1669,2
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,3,4,0,2,0,2,0,0,15.56,26,6,0.3856,25
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,4,5,0,3,0,0,0,0,18.56,35,35,1.8858,6
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,5,17,0,1,0,0,0,0,13.54,88,88,6.4993,10
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,6,23,0,8,0,1,0,0,22.15,139,129,5.8239,4
```
SCSA
```csv
scsa_id,firstname,lastname,match_name,match_date,stage_number,stage_total_time,string_1_time,string_1_penalty,string_1_dnf,string_2_time,string_2_penalty,string_2_dnf,string_3_time,string_3_penalty,string_3_dnf,string_4_time,string_4_penalty,string_4_dnf,string_5_time,string_5_penalty,string_5_dnf
member-id-1,firstname-1,lastname-1,ClubName Steel Challenge - December 2025 Saturday Match,20251220,1,14.85,4.29,0.0,False,3.32,0.0,False,3.38,0.0,False,5.28,0.0,False,3.86,0.0,False
member-id-1,firstname-1,lastname-1,ClubName Steel Challenge - December 2025 Saturday Match,20251220,2,13.17,3.78,0.0,False,5.65,1.0,False,3.19,0.0,False,3.53,0.0,False,2.67,0.0,False
member-id-1,firstname-1,lastname-1,ClubName Steel Challenge - December 2025 Saturday Match,20251220,3,14.1,3.79,0.0,False,3.31,0.0,False,3.78,0.0,False,3.22,0.0,False,3.83,0.0,False
member-id-1,firstname-1,lastname-1,ClubName Steel Challenge - December 2025 Saturday Match,20251220,4,24.05,11.64,0.0,False,30.0,0.0,False,3.89,0.0,False,4.53,0.0,False,3.99,0.0,False
member-id-1,firstname-1,lastname-1,ClubName Steel Challenge - December 2025 Saturday Match,20251220,5,16.82,6.84,0.0,False,5.51,0.0,False,4.18,0.0,False,3.42,0.0,False,3.71,0.0,False
member-id-1,firstname-1,lastname-1,ClubName Steel Challenge - December 2025 Saturday Match,20251220,6,11.03,5.09,0.0,False,3.27,0.0,False,2.63,0.0,False,2.54,0.0,False,2.59,0.0,False
```
