# practiscore-export-parser

**This project is not affiliiated with practiscore. This is a personal project 
that parses individual competitor's data from practiscore output**

## ps-tap
`ps-tap` extracts data from the web report export based on uspsa member_id and outputs a csv file

```sh
Usage: ps-tap --uspsa-id USPSA-ID --report REPORT_PATH
              --output FILE TO OUTPUT INDIVIDUAL SHOOTER DATA

Available options:
  -h,--help                Show this help text
  --uspsa-id USPSA-ID      USPSA MEMBER ID
  --report REPORT_PATH     Relative path of the report
  --output FILE TO OUTPUT INDIVIDUAL SHOOTER DATA
                           Relative path to output individual report
```

***Example Output***
```csv
uspsa_id,firstname,lastname,class,division,match_points,place_overall,gun,stage,A,B,C,D,miss,no_shoot,procedural,time,raw_points,total_points,hit_factor,stage_place
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,1,24,0,1,0,3,0,0,19.61,123,93,4.7425,11
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,2,15,0,9,0,0,0,0,16.54,102,102,6.1669,2
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,3,4,0,2,0,2,0,0,15.56,26,6,0.3856,25
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,4,5,0,3,0,0,0,0,18.56,35,35,1.8858,6
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,5,17,0,1,0,0,0,0,13.54,88,88,6.4993,10
uspsa-member-id,firstname,lastname,B,Limited Optics,449.9983,7,Pistol,6,23,0,8,0,1,0,0,22.15,139,129,5.8239,4
```
