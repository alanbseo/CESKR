library(data.table)
library(doSNOW)
library(openxlsx)
library(readxl)
library(rgdal)
# library(sf)
# devtools::install_github("cardiomoon/Kormaps")
library(Kormaps)
library(dplyr)
library(stringr)



path_data = "~/Dropbox/KIT/생태원/2023 과제/Data/"
path_out = "~/Dropbox/KIT/생태원/2023 과제/tmp/"
setwd(path_data)


path_gis= "~/Dropbox/GIS Data/Korea/"


# # LSMD 
# path_lsmd = "~/Dropbox/GIS Data/Korea/Admin/LSMD_ADMIN/"
# 
# lsmd_seoul = readOGR(paste0(path_lsmd, "LSMD_ADM_SECT_UMD_11_Seoul/LSMD_ADM_SECT_UMD_11.shp"), use_iconv = TRUE, encoding = "EUC-KR") # 
# 
# head(lsmd_seoul@data)


# 2022 data
# survey_dt = read_xlsx("생태계서비스팀 공간정보/10. 생태계서비스/2. 생태계서비스 대국민 인식조사 설문결과_19May2023수정.xlsx")
# 2023 data
survey_dt_raw = read_xlsx("생태계서비스팀 공간정보/10. 생태계서비스/Data_생태계서비스 인식조사_합본_230704.xlsx", sheet = 2)

colnames(survey_dt_raw)

survey_dt = survey_dt_raw[!is.na(survey_dt_raw$No),]

nrow(survey_dt) # 14688

n_cols = 59 
survey_dt = survey_dt[, 1:n_cols]
# colnames(survey_dt)[1] = "Year"
colnames(survey_dt)[3] = "Province_Code"
colnames(survey_dt)[4] = "Province"

colnames(survey_dt)[5] = "Address1"
colnames(survey_dt)[6] = "Address2"
colnames(survey_dt)[7] = "Address3"

# colnames(survey_dt)[8] = "ResidenceYear" # Q3
# colnames(survey_dt)[9] = "ResidenceType" # Q4

colnames(survey_dt)[58] = "Gender"
colnames(survey_dt)[59] = "Age"

colnames(survey_dt)
# Q15-Q35: continuous

table(survey_dt$Province) # Province

table(survey_dt$Address1)
# table(survey_dt$Address2) 
# table(survey_dt$Address3)




### 시군구

# filepath_SGG = "생태계서비스팀 공간정보/01. 각종 경계모음/통계청_행정구역(2022)/시군구행정경계_전국.shp"
# sgg_shp = readOGR(filepath_SGG, use_iconv = TRUE, encoding = "EUC-KR") #

# http://www.gisdeveloper.co.kr/?p=2332
# 법정동 기반 시군구
filepath_SGG2 = "~/Dropbox/GIS Data/Korea/Admin/SIG_202302/sig.shp"
sgg_shp = readOGR(filepath_SGG2, use_iconv = TRUE, encoding = "EUC-KR") #

# # 법정동 기반 시군구 단위
# sgg_bjd_df = read_xlsx(paste0(path_gis, "Admin/법정동 기준 시군구 단위.xlsx"),4)
# 
# 
# stopifnot(all(sgg_shp$SIG_CD %in% sgg_bjd_df$SGG_CODE_BJD))




### 법정동 
filepath_admincode = "~/Dropbox/GIS Data/Korea/Admin/한국행정구역코드.xlsx"
admincode_dt = read_xlsx(filepath_admincode, sheet = 4, skip = 1) # fourth sheet
head(admincode_dt)
colnames(admincode_dt)
colnames(admincode_dt) = c("Region", "SGG", "HJD_Kor", "HJD_2nd", "BJD", "HJD_Code", "HJD_2nd_Code", "HJD_Date", "BJD_Code", "BJD_Area", "HJD_Eng", "Note")

# SGIS 행정구역코드
# filepath_sggcode = "~/Dropbox/GIS Data/Korea/Admin/adm_code.xls"
# sggcode_dt = read_xls(filepath_sggcode, sheet = 1, skip = 1) # first sheet
# head(sggcode_dt)
# colnames(sggcode_dt)
# colnames(sggcode_dt) = c("Region_Code", "Region", "SGG_CodeShort", "SGG", "EMD_Code", "EMD")
# 
# 
# # padding 
# sggcode_dt$SGG_CodeShort = formatC(as.numeric(sggcode_dt$SGG_CodeShort), width = 3, format = "d", flag = "0")
# 
# sggcode_dt$SGG_Code = paste0(sggcode_dt$Region_Code, sggcode_dt$SGG_CodeShort )


### read admin boundary shp 

# filepath_emd = "~/Dropbox/GIS Data/Korea/Admin/EMD_202302/emd.shp"
# emd = readOGR(filepath_emd, use_iconv = TRUE, encoding = "EUC-KR") #
# # # emd$EMD_CD
# # # (emd$EMD_KOR_NM)
# 
# filepath_lard = "~/Dropbox/GIS Data/Korea/Admin/LARD_ADMIN/"
# list.dirs(filepath_lard, recursive = F)

# lard_seoul = readOGR(paste0(filepath_lard, "LARD_ADM_SECT_SGG_11_Seoul/LARD_ADM_SECT_SGG_11.shp"), use_iconv = TRUE, encoding = "EUC-KR") # 
# 
# # ADM_SECT_CD	행정구역코드
# # SGG_NM	시군구명
# # SGG_OID	원천오브젝트ID
# # COL_ADM_SECT_CD	원천시군구코드
# # OBJECTID	OBJECTID
# # SHAPE	도형정보
# 
# lard_seoul$GID
# lard_seoul$ADM_SECT_C
# lard_seoul$SGG_NM
# lard_seoul$COL_ADM_SE

table(survey_dt$Province, survey_dt$Province_Code)

RegionCode_old = c(1:17)
RegionCode_new = c(11, 28, 30, 29, 27, 31, 26, 36, 50, 41, 42, 43, 44, 45, 46, 47, 48)
regionname_kor = c("서울특별시", "인천광역시", "대전광역시", "광주광역시", "대구광역시", "울산광역시", "부산광역시", "세종특별자치시", "제주특별자치도", "경기도", "강원도", "충청북도", "충청남도", "전라북도", "전라남도", "경상북도", "경상남도")
regionname_eng = c("Seoul", "Incheon", "Daejeon", "Gwangju", "Daegu", "Ulsan", "Busan", "Sejon", "Jeju", "Gyeonggi", "Gangwon", "Chungbuk", "Chungnam", "Jeonbuk", "Jeonnam", "Gyeongbuk", "Gyeongnam")
regionname_kor_old = c("서울", "인천", "대전", "광주", "대구", "울산", "부산", "세종특별자치시", "제주 제주특별자치도", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남")




# survey_dt[survey_dt$Province == "부산" & survey_dt$Province_Code == 3, ]$Province_Code = 7 # Obs 7019
# 
# survey_dt[survey_dt$Province == "전북" & survey_dt$Province_Code == 15, ]$Province_Code = 14 # Obs 12741

# 코드와 지역명 일치시키기
for (r_idx in seq_along(regionname_eng)) { 
    
    dt1 = survey_dt[survey_dt$Province == regionname_kor_old[r_idx] & survey_dt$Province_Code != r_idx, ]
    print(nrow(dt1))
    
    survey_dt[survey_dt$Province == regionname_kor_old[r_idx] & survey_dt$Province_Code != r_idx, ]$Province_Code = r_idx
}




survey_dt$RegionCode = RegionCode_new[survey_dt$Province_Code]


admincode_dt$RegionCode = RegionCode_new[match(admincode_dt$Region,regionname_kor)]



table(is.na(admincode_dt$RegionCode ))



# TX3_3_spl = foreach (tx3_3 = survey_dt$TX3_3_RE, .combine = "rbind") %do% { 
#     
#     spl = strsplit(tx3_3, " ")[[1]]
#     if (length(spl) == 1) {
#         spl = c(spl, "")   
#     } else if (length(spl) == 2) {
#         # do nothing
#         
#     } else if (length(spl) == 3) {
#         spl = c(spl[1], paste(spl[2], spl[3])   )
#         
#     } else {
#         stop("shouln't be more than three elements")
#         
#     }
#     
#     return(spl)
# }
# 
# colnames(TX3_3_spl) = c("Region", "SGG")






survey_dt_new = data.frame(ObsID = 1:nrow(survey_dt), survey_dt) #  cbind(survey_dt, TX3_3_spl)


survey_dt_new[is.na(survey_dt_new$Address2),]$Address2 = "" # 
survey_dt_new[survey_dt_new$Address2 == "전체" ,]$Address2 = "" # 
survey_dt_new[survey_dt_new$Address2 == "없음" ,]$Address2 = "" # 

# 원본 자료 수정
survey_dt_new[survey_dt_new$Address2 == "화곡동" & survey_dt_new$Address1 == "상서구"  ,]$Address1 = "강서구" # 강서구
survey_dt_new[survey_dt_new$Address2 == "용현동" & survey_dt_new$Address1 == "남구"  ,]$Address1 = "미추홀구" # 미추홀구
survey_dt_new[survey_dt_new$Address2 == "인수" & survey_dt_new$Address1 == "강북구"  ,]$Address2 = "수유동" # 




survey_dt_new[survey_dt_new$Address2 == "송천동" & survey_dt_new$Address1 == "전주시"  ,]$Address2 = "송천동1가" # 
# 1-2가 있음
survey_dt_new[survey_dt_new$Address2 == "효자동" & survey_dt_new$Address1 == "전주시"  ,]$Address2 = "효자동1가" # 
# 1-3가 있음

survey_dt_new[survey_dt_new$Address2 == "우아동" & survey_dt_new$Address1 == "전주시"  ,]$Address2 = "우아동1가" # 
# 1-3가 있음
survey_dt_new[survey_dt_new$Address2 == "덕진동" & survey_dt_new$Address1 == "전주시"  ,]$Address2 = "덕진동1가" # 
# 1-3가 있음
survey_dt_new[survey_dt_new$Address2 == "삼천동" & survey_dt_new$Address1 == "전주시"  ,]$Address2 = "삼천동1가" # 
# 1-3가 있음
survey_dt_new[survey_dt_new$Address2 == "중화산동" & survey_dt_new$Address1 == "전주시"  ,]$Address2 = "중화산동1가" # 
# 1-3가 있음



survey_dt_new[survey_dt_new$Address2 == "계양동" & survey_dt_new$Address1 == "계양구"  ,]$Address2 = "박촌동" # 행정동 . 다른 동도 있음



survey_dt_new[survey_dt_new$Address2 == "원효동" & survey_dt_new$Address1 == "용산구"  ,]$Address2 = "원효로1가" # 

survey_dt_new[survey_dt_new$Address2 == "신길제1동" & survey_dt_new$Address1 == "영등포구"  ,]$Address2 = "신길동" # 


survey_dt_new[survey_dt_new$Address2 == "신월로" & survey_dt_new$Address1 == "양천구"  ,]$Address2 = "신월동" # 

survey_dt_new[survey_dt_new$Address2 == "신정로" & survey_dt_new$Address1 == "양천구"  ,]$Address2 = "신정동" # 
survey_dt_new[survey_dt_new$Address2 == "테헤란로" & survey_dt_new$Address1 == "강남구"  ,]$Address2 = "역삼동" # 
survey_dt_new[survey_dt_new$Address2 == "도봉산동" & survey_dt_new$Address1 == "강동구"  ,]$Address2 = "" # 오타 아마도
survey_dt_new[survey_dt_new$Address2 == "하곡동" & survey_dt_new$Address1 == "강서구"  ,]$Address2 = "화곡동" # 오타 아마도 
survey_dt_new[survey_dt_new$Address2 == "과인ㆍ루로" & survey_dt_new$Address1 == "광진구"  ,]$Address2 = "구의동" # 길이름

survey_dt_new[survey_dt_new$Address2 == "신수로" & survey_dt_new$Address1 == "마포구"  ,]$Address2 = "신수동" # 길이름
survey_dt_new[survey_dt_new$Address2 == "방배본" & survey_dt_new$Address1 == "서초구"  ,]$Address2 = "방배동" # 행정동
survey_dt_new[survey_dt_new$Address2 == "왕십리동" & survey_dt_new$Address1 == "성동구"  ,]$Address2 = "상왕십리동" # 하왕십리동도 있음
survey_dt_new[survey_dt_new$Address2 == "돌곶이로" & survey_dt_new$Address1 == "성북구"  ,]$Address2 = "석관동" # 길이름
survey_dt_new[survey_dt_new$Address2 == "운암" & survey_dt_new$Address1 == "성북구"  ,]$Address2 = "응암동" #
survey_dt_new[survey_dt_new$Address2 == "동소문동" & survey_dt_new$Address1 == "성북구"  ,]$Address2 = "동소문동1가" # 

survey_dt_new[survey_dt_new$Address2 == "일광읍" & survey_dt_new$Address1 == "기장군"  ,]$Address2 = "일광면" # 

survey_dt_new[survey_dt_new$Address2 == "말우" & survey_dt_new$Address1 == "중랑구"  ,]$Address2 = "망우동" # 
survey_dt_new[survey_dt_new$Address2 == "수영" & survey_dt_new$Address1 == "수영구"  ,]$Address2 = "수영구" # 
survey_dt_new[survey_dt_new$Address2 == "영도" & survey_dt_new$Address1 == "영도구"  ,]$Address2 = "영도구" # 
survey_dt_new[survey_dt_new$Address2 == "덕양ㅈ검사소" & survey_dt_new$Address1 == "고양시"  ,]$Address2 = "행신동" # 


survey_dt_new[survey_dt_new$Address2 == "삼도동" & survey_dt_new$Address1 == "제주시"  ,]$Address2 = "삼도일동" # 
survey_dt_new[survey_dt_new$Address1 == "세종동"& survey_dt_new$Province == "세종특별자치시"  ,]$Address2 = "세종특별자치시" #
survey_dt_new[is.na(survey_dt_new$Address1)& survey_dt_new$Province == "세종"  ,][c("Address1", "Address2")] = c( "세종특별자치시", "세종특별자치시")


# survey_dt_new[survey_dt_new$Address2 == "말우" & survey_dt_new$Address1 == "중랑구"  ,]$Address2 = "망우동" # 
# survey_dt_new[survey_dt_new$Address2 == "말우" & survey_dt_new$Address1 == "중랑구"  ,]$Address2 = "망우동" # 
# survey_dt_new[survey_dt_new$Address2 == "말우" & survey_dt_new$Address1 == "중랑구"  ,]$Address2 = "망우동" # 
# survey_dt_new[survey_dt_new$Address2 == "말우" & survey_dt_new$Address1 == "중랑구"  ,]$Address2 = "망우동" # 
# survey_dt_new[survey_dt_new$Address2 == "말우" & survey_dt_new$Address1 == "중랑구"  ,]$Address2 = "망우동" # 
# 


survey_dt_new[survey_dt_new$Address2 == "서남로" & survey_dt_new$Address1 == "남구"  ,]$Address2 = "서석동" # 


survey_dt_new[survey_dt_new$Address2 == "금호동" & survey_dt_new$Address1 == "성동구"  ,]$Address2 = "금호동1가" # 
# 1-4가 있음

survey_dt_new[survey_dt_new$Address2 == "성수동" & survey_dt_new$Address1 == "성동구"  ,]$Address2 = "성수동1가" # 
# 성수동 1가와 2가 있음

survey_dt_new[survey_dt_new$Address2 == "서초동" & survey_dt_new$Address1 == "강남구"  ,]$Address1 = "서초구" # 
survey_dt_new[survey_dt_new$Address2 == "성동" & survey_dt_new$Address1 == "강남구"  ,][, c("Address1", "Address2")] = c("성동구", "")# 


survey_dt_new[survey_dt_new$Address2 == "신촌동" & survey_dt_new$Address1 == "안양시"  ,][, c("Address1", "Address2")] = c("안양시 만안구", "호계동")# 
survey_dt_new[survey_dt_new$Address2 == "1동" & survey_dt_new$Address1 == "안양시"  ,][, c("Address1", "Address2")] = c("안양시 만안구", "안양동")# 
survey_dt_new[survey_dt_new$Address2 == "범계동" & survey_dt_new$Address1 == "안양시"  ,][, c("Address1", "Address2")] = c("안양시 동안구", "호계동")# 
survey_dt_new[survey_dt_new$Address2 == "부흥동" & survey_dt_new$Address1 == "안양시"  ,][, c("Address1", "Address2")] = c("안양시 동안구", "비산동")# 
survey_dt_new[survey_dt_new$Address2 == "인덕원동" & survey_dt_new$Address1 == "안양시"  ,][, c("Address1", "Address2")] = c("안양시 동안구", "관양동")# 신규 행정동
survey_dt_new[survey_dt_new$Address2 == "연평" & survey_dt_new$Address1 == "안양시"  ,][, c("Address1", "Address2")] = c("안양시", "안양시")#  없는 지명



survey_dt_new[survey_dt_new$Address2 == "송림" & survey_dt_new$Address1 == "미추홀구"  ,][, c("Address1", "Address2")] = c("동구", "송림동")# 


survey_dt_new[survey_dt_new$Address2 == "단원" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1", "Address2")] = c("안산시 단원구", "안산시 단원구")# 
survey_dt_new[survey_dt_new$Address2 == "배곧동" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1", "Address2")] = c("시흥시", "정왕동")# 
survey_dt_new[survey_dt_new$Address2 == "백운동" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1", "Address2")] = c("안산시 단원구", "원곡동")# 행정동
survey_dt_new[survey_dt_new$Address2 == "사이동" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1", "Address2")] = c("안산시 상록구", "사동")# 행정동

survey_dt_new[survey_dt_new$Address2 == "신현동" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1", "Address2")] = c("시흥시", "방산동")# 시흥시행정동. 다른 동도 있음
survey_dt_new[survey_dt_new$Address2 == "호수동" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1", "Address2")] = c("안산시 단원구", "고잔동")# 행정동
survey_dt_new[survey_dt_new$Address2 == "해양동" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1", "Address2")] = c("안산시 상록구", "사동")# 행정동

survey_dt_new[survey_dt_new$Address2 == "화랑로" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1", "Address2")] = c("안산시 단원구", "고잔동")# 행정동





survey_dt_new[survey_dt_new$Address2 == "기흥수" & survey_dt_new$Address1 == "용인시"  ,][, c("Address1", "Address2")] = c("용인시 기흥구", "공세동")# 행정동

survey_dt_new[survey_dt_new$Address2 == "기흥동" & survey_dt_new$Address1 == "용인시"  ,][, c("Address1", "Address2")] = c("용인시 기흥구", "공세동")# 행정동

survey_dt_new[survey_dt_new$Address2 == "유림동" & survey_dt_new$Address1 == "용인시"  ,][, c("Address1", "Address2")] = c("용인시 처인구", "유방동")# 헹정동

survey_dt_new[survey_dt_new$Address2 == "고링동" & survey_dt_new$Address1 == "용인시"  ,][, c("Address1", "Address2")] = c("용인시 처인구", "고림동")# 오타




survey_dt_new[survey_dt_new$Address2 == "구성동" & survey_dt_new$Address1 == "용인시"  ,][, c("Address1", "Address2")] = c("용인시 기흥구", "언남동")# 행정동. 



survey_dt_new[survey_dt_new$Address2 == "이동" & survey_dt_new$Address1 == "용인시"  ,][, c("Address1", "Address2")] = c("용인시 처인구", "이동읍")# 

survey_dt_new[survey_dt_new$Address2 == "평화동" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1", "Address2")] = c("전주시 완산구", "평화동1가")# 행정동. 완산구 

survey_dt_new[survey_dt_new$Address2 == "중앙동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1", "Address2")] = c("수원시 팔달구", "중동")# 오타인 듯

# 
# survey_dt_new[survey_dt_new$Address2 == "중앙동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1")] = "수원시 영통구"
survey_dt_new[survey_dt_new$Address2 == "중동" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1")] = "전주시 덕진구"
survey_dt_new[survey_dt_new$Address2 == "인후동" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1", "Address2")] = c("전주시 덕진구", "인후동1가")
survey_dt_new[survey_dt_new$Address2 == "호성동" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1", "Address2")] = c("전주시 덕진구", "호성동1가") # 행정동. 다른 동 있음.

survey_dt_new[survey_dt_new$Address2 == "혁신동" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1", "Address2")] = c("전주시 덕진구", "만성동") # 행정동. 다른 동 있음.
survey_dt_new[survey_dt_new$Address2 == "삼례" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1", "Address2")] = c("완주군", "삼례읍") # 
survey_dt_new[survey_dt_new$Address2 == "완산구" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1", "Address2")] = c("전주시 완산구", "전주시 완산구") #  
survey_dt_new[survey_dt_new$Address2 == "완산" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1", "Address2")] = c("전주시 완산구", "전주시 완산구") #  
survey_dt_new[survey_dt_new$Address2 == "숲정이" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1", "Address2")] = c("전주시 덕진구", "진북동") # 숲정이 순교성지  


survey_dt_new[survey_dt_new$Address2 == "가수원" & survey_dt_new$Province == "대전"  ,][, c("Address2")] = "가수원동"



survey_dt_new[survey_dt_new$Address2 == "동대신동" & survey_dt_new$Address1 == "서구"  ,][, c("Address2")] = "가수원동"
survey_dt_new[survey_dt_new$Address2 == "운정동" & survey_dt_new$Address1 == "파주시"  ,][, c("Address2")] = "교하동" # 행정동. 다른 동도 있음



survey_dt_new[survey_dt_new$Address2 == "은평" & survey_dt_new$Address1 == "은평구"  ,][, c("Address2")] = "은평구"
survey_dt_new[survey_dt_new$Address2 == "진흥로" & survey_dt_new$Address1 == "은평구"  ,][, c("Address2")] = "응암동"
survey_dt_new[survey_dt_new$Address2 == "수성동" & survey_dt_new$Address1 == "수성구"  ,][, c("Address2")] = "수성동1가" # 다른 동도 있음


survey_dt_new[survey_dt_new$Address2 == "하동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1")] = "수원시 영통구"
survey_dt_new[survey_dt_new$Address2 == "신동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1")] = "수원시 영통구"

survey_dt_new[survey_dt_new$Address2 == "정자동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1")] = "수원시 장안구"
survey_dt_new[survey_dt_new$Address2 == "금암동" & survey_dt_new$Address1 == "전주시"  ,][, c("Address1")] = "전주시 덕진구"
survey_dt_new[survey_dt_new$Address2 == "부곡동" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1")] = "안산시 상록구"
survey_dt_new[survey_dt_new$Address2 == "고등동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1")] = "수원시 팔달구"

survey_dt_new[survey_dt_new$Address2 == "우만동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1")] = "수원시 팔달구"
survey_dt_new[survey_dt_new$Address2 == "성남동" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1")] = "성남시 중원구"

survey_dt_new[survey_dt_new$Address2 == "수진2동" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1", "Address2")] =c("성남시 수정구", "수진동")
survey_dt_new[survey_dt_new$Address2 == "운주동" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1", "Address2")] =c("성남시 분당구", "운중동") # 오타
survey_dt_new[survey_dt_new$Address2 == "태펑동" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1", "Address2")] =c("성남시 수정구", "태평동") # 오타
survey_dt_new[survey_dt_new$Address2 == "동판교로" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1", "Address2")] =c("성남시 분당구", "백현동") # 길이름. 다른 동도 있음



survey_dt_new[survey_dt_new$Address2 == "신흥동" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1")] = "성남시 수정구"
survey_dt_new[survey_dt_new$Address2 == "성북로" & survey_dt_new$Address1 == "북구"  ,][, c("Address2")] = "침산동" # 길이름

survey_dt_new[survey_dt_new$Address2 == "우동" & survey_dt_new$Address1 == "수성구"  ,][, c("Address2")] = "" # 없는 지명

survey_dt_new[survey_dt_new$Address2 == "친추" & survey_dt_new$Address1 == "수성구"  ,][, c("Address2")] = "" # 없는 지명

survey_dt_new[survey_dt_new$Address2 == "계산동" & survey_dt_new$Address1 == "중구"  ,][, c("Address2")] = "계산동1가" #

survey_dt_new[survey_dt_new$Address2 == "달성로" & survey_dt_new$Address1 == "중구"  ,][, c("Address2")] = "달성동" #
survey_dt_new[survey_dt_new$Address2 == "명덕로" & survey_dt_new$Address1 == "중구"  ,][, c("Address2")] = "범어동" #
survey_dt_new[survey_dt_new$Address2 == "상열" & survey_dt_new$Address1 == "군포시"  ,][, c("Address2")] = "" # 오타
survey_dt_new[survey_dt_new$Address2 == "신본동" & survey_dt_new$Address1 == "군포시"  ,][, c("Address2")] = "산본동" # 오타
survey_dt_new[survey_dt_new$Address2 == "감정면" & survey_dt_new$Address1 == "김포시"  ,][, c("Address2")] = "감정동" # 

survey_dt_new[survey_dt_new$Address2 == "죽산" & survey_dt_new$Address1 == "안성시"  ,][, c("Address2")] = "죽산면" # 

survey_dt_new[survey_dt_new$Address2 == "회천동" & survey_dt_new$Address1 == "양주시"  ,][, c("Address2")] = "덕정동" #  행정동. 다른 동도 있음
survey_dt_new[survey_dt_new$Address2 == "아미" & survey_dt_new$Address1 == "이천시"  ,][, c("Address2")] = "아미리" #  
survey_dt_new[survey_dt_new$Address2 == "월롱" & survey_dt_new$Address1 == "파주시"  ,][, c("Address2")] = "월롱면" #  
survey_dt_new[survey_dt_new$Address2 == "동탄면" & survey_dt_new$Address1 == "화성시"  ,][, c("Address2")] = "반송동" #  행정동

survey_dt_new[survey_dt_new$Address2 == "서정돝" & survey_dt_new$Address1 == "평택시"  ,][, c("Address2")] = "서정동" #  
survey_dt_new[survey_dt_new$Address2 == "양양" & survey_dt_new$Address1 == "양양군"  ,][, c("Address2")] = "양양읍" #  
survey_dt_new[survey_dt_new$Address2 == "맹동" & survey_dt_new$Address1 == "음성군"  ,][, c("Address2")] = "맹동면" #  

survey_dt_new[survey_dt_new$Address2 == "읍성읍" & survey_dt_new$Address1 == "음성군"  ,][, c("Address2")] = "음성읍" #  
survey_dt_new[survey_dt_new$Address2 == "부성2동" & survey_dt_new$Address1 == "천안시"  ,][, c("Address2")] = "두정동" #  행정동
survey_dt_new[survey_dt_new$Address2 == "중앙로" & survey_dt_new$Address1 == "고창군"  ,][, c("Address2")] = "고창읍" # 
survey_dt_new[survey_dt_new$Address2 == "장수" & survey_dt_new$Address1 == "장수군"  ,][, c("Address2")] = "장수읍" # 

survey_dt_new[survey_dt_new$Address2 == "옹동" & survey_dt_new$Address1 == "정읍시"  ,][, c("Address2")] = "옹동면" # 
survey_dt_new[survey_dt_new$Address2 == "곡성" & survey_dt_new$Address1 == "곡성군"  ,][, c("Address2")] = "곡성읍" # 
survey_dt_new[survey_dt_new$Address2 == "영나로" & survey_dt_new$Address1 == "나주시"  ,][, c("Address2")] = "용산동" #  길이름
survey_dt_new[survey_dt_new$Address2 == "하당로" & survey_dt_new$Address1 == "목포시"  ,][, c("Address2")] = "옥암동" #  길이름

survey_dt_new[survey_dt_new$Address2 == "도포면당산길" & survey_dt_new$Address1 == "영암군"  ,][, c("Address2")] = "도포면" #  길이름
survey_dt_new[survey_dt_new$Address2 == "진람읍" & survey_dt_new$Address1 == "경산시"  ,][, c("Address2")] = "진량읍" #  오타
survey_dt_new[survey_dt_new$Address2 == "대릉" & survey_dt_new$Address1 == "경주시"  ,][, c("Address2")] = "황남동" #  오타
survey_dt_new[survey_dt_new$Address2 == "싱모" & survey_dt_new$Address1 == "구미시"  ,][, c("Address2")] = "상모동" #  오타
survey_dt_new[survey_dt_new$Address2 == "송하" & survey_dt_new$Address1 == "성주군"  ,][, c("Address2")] = "" #  오타

survey_dt_new[survey_dt_new$Address2 == "송현" & survey_dt_new$Address1 == "성주군"  ,][, c("Address2")] = "" #  오타
survey_dt_new[survey_dt_new$Address2 == "용상" & survey_dt_new$Address1 == "성주군"  ,][, c("Address2")] = "" #  오타

survey_dt_new[survey_dt_new$Address2 == "중구" & survey_dt_new$Address1 == "성주군"  ,][, c("Address2")] = "" #  오타
survey_dt_new[survey_dt_new$Address2 == "두동면" & survey_dt_new$Address1 == "거제시"  ,][, c("Address1", "Province", "Province_Code", "RegionCode")] = c("울주군", "울산", 6, 31) #  오타
survey_dt_new[survey_dt_new$Address2 == "삼성동" & survey_dt_new$Address1 == "영주시"  ,][, c("Address2")] = "" #  없는 지명

survey_dt_new[survey_dt_new$Address2 == "천정동" & survey_dt_new$Address1 == "진주시"  ,][, c("Address2")] = "망경동" #  행정동. 다른 동도 있음
survey_dt_new[survey_dt_new$Address2 == "반월중앙동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address2")] = "반월동" #  행정동. 다른 동도 있음

survey_dt_new[survey_dt_new$Address2 == "양덕1동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address2")] = "양덕동" #  행정동. 
survey_dt_new[survey_dt_new$Address2 == "웅천동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address2")] = "북부동" #   다른 동도 있음
survey_dt_new[survey_dt_new$Address2 == "의창동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address2")] = "북동" #   다른 동도 있음
survey_dt_new[survey_dt_new$Address2 == "중앙북1길" & survey_dt_new$Address1 == "창원시"  ,][, c("Address2")] = "중앙동3가" # 길이름.  다른 동도 있음



survey_dt_new[survey_dt_new$Address2 == "임동" & survey_dt_new$Address1 == "성주군"  ,][, c("Address2")] = "" #  오타. 아니면 안동시 임동면?

survey_dt_new[survey_dt_new$Address2 == "신안동" & survey_dt_new$Address1 == "천안시"  ,][, c("Address2")] = "원성동" #  행정동

survey_dt_new[survey_dt_new$Address2 == "괴산" & survey_dt_new$Address1 == "청주시"  ,][, c("Address1", "Address2")] = c("괴산군", "괴산읍") #  
survey_dt_new[survey_dt_new$Address2 == "내숭" & survey_dt_new$Address1 == "청주시"  ,][, c("Address1", "Address2")] = c("청주시 청원구", "내수읍") #  
survey_dt_new[survey_dt_new$Address2 == "운동" & survey_dt_new$Address1 == "청주시"  ,][, c("Address1", "Address2")] = c("청주시", "청주시") #  오타


survey_dt_new[survey_dt_new$Address2 == "싱덕면" & survey_dt_new$Address1 == "임실군"  ,][, c("Address2")] = "신덕면" #  

survey_dt_new[survey_dt_new$Address2 == "서남구" & survey_dt_new$Address1 == "동구"  ,][, c("Address2")] = "용운동" # 대전 동구 선암로?


survey_dt_new[survey_dt_new$Address2 == "성남시" & survey_dt_new$Address1 == "동구"  ,][, c("Address2")] = "" # 오타 아마도

survey_dt_new[survey_dt_new$Address2 == "장곡로" & survey_dt_new$Address1 == "의정부시"  ,][, c("Address2")] = "장암동" #

survey_dt_new[survey_dt_new$Address2 == "강동산하2로" & survey_dt_new$Address1 == "북구"  ,][, c("Address2")] = "산하동" # 울산
survey_dt_new[survey_dt_new$Address2 == "행주동" & survey_dt_new$Address1 == "고양시"  ,][, c("Address1", "Address2")] = c("고양시 덕양구", "토당동") #  행정동


survey_dt_new[survey_dt_new$Address2 == "인림" & survey_dt_new$Address1 == "관악구"  ,][, c("Address2")] = "신림동" #  
survey_dt_new[survey_dt_new$Address2 == "구로" & survey_dt_new$Address1 == "관악구"  ,][, c("Address1", "Address2")] = c("구로구", "구로동") #  

survey_dt_new[survey_dt_new$Address2 == "내당" & survey_dt_new$Address1 == "북구"  ,][, c("Address1", "Address2")] = c("서구", "내당동") #  

survey_dt_new[survey_dt_new$Address2 == "산흥동" & survey_dt_new$Address1 == "동구"  ,][, c("Address1", "Address2")] = c("동구", "신흥동") #  

survey_dt_new[survey_dt_new$Address2 == "능평동" & survey_dt_new$Address1 == "광주시"  ,][, c("Address2")] = "능평리" #  
survey_dt_new[survey_dt_new$Address2 == "양벌동" & survey_dt_new$Address1 == "광주시"  ,][, c("Address2")] = "양벌리" #  
survey_dt_new[survey_dt_new$Address2 == "매산동" & survey_dt_new$Address1 == "광주시"  ,][, c("Address2")] = "매산리" #  
survey_dt_new[survey_dt_new$Address2 == "신현동" & survey_dt_new$Address1 == "광주시"  ,][, c("Address2")] = "신현리" #  
survey_dt_new[survey_dt_new$Address2 == "추자동" & survey_dt_new$Address1 == "광주시"  ,][, c("Address2")] = "추자리" #  
survey_dt_new[survey_dt_new$Address2 == "쌍형동" & survey_dt_new$Address1 == "광주시"  ,][, c("Address2")] = "쌍령동" #  


survey_dt_new[survey_dt_new$Address2 == "곡반정" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1", "Address2")] = c("수원시 권선구", "곡반정동") #  

survey_dt_new[survey_dt_new$Address2 == "광교동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1", "Address2")] = c("수원시 장안구", "연무동") # 혹은 영통구 광교1동.. 

survey_dt_new[survey_dt_new$Address2 == "덕면동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address2")] = "" # 없는 지명
 

survey_dt_new[survey_dt_new$Address2 == "대청로" & survey_dt_new$Address1 == "옹진군"  ,][, c("Address2")] = "대청면" # 없는 지명

survey_dt_new[survey_dt_new$Address2 == "대청동" & survey_dt_new$Address1 == "옹진군"  ,][, c("Address2")] = "대청면" # 없는 지명

survey_dt_new[survey_dt_new$Address2 == "여후동" & survey_dt_new$Address1 == "연수구"  ,][, c("Address2")] = "대청면" #  

survey_dt_new[survey_dt_new$Address2 == "권선로" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1", "Address2")] = c("수원시 권선구", "권선동") #


survey_dt_new[survey_dt_new$Address2 == "팔달동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1", "Address2")] = c("수원시 팔달구", "팔달로1가") # 다른 동도 있음.



survey_dt_new[survey_dt_new$Address2 == "필달" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1", "Address2")] = c("수원시 팔달구", "팔달로1가") # 다른 동도 있음.


survey_dt_new[survey_dt_new$Address2 == "곡선동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1", "Address2")] = c("수원시 권선구", "곡반정동") #
survey_dt_new[survey_dt_new$Address2 == "율천동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1", "Address2")] = c("수원시 장안구", "율전동") # 율전동 천천동

survey_dt_new[survey_dt_new$Address2 == "행궁동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1", "Address2")] = c("수원시 팔달구", "팔달로1가") # 행정동. 다른 동도 있음.


survey_dt_new[survey_dt_new$Address2 == "고덕동" & survey_dt_new$Address1 == "평택시"  ,][, c("Address2")] = "고덕면" #  


survey_dt_new[survey_dt_new$Address2 == "오금동" & survey_dt_new$Address1 == "군포시"  ,][, c("Address2")] = "산본동" # 행정동. 다른 동도 있음.



survey_dt_new[survey_dt_new$Address2 == "일산구" & survey_dt_new$Address1 == "고양시"  ,][, c("Address2")] = "고양시 일산동구" # 일산서구도 있음

survey_dt_new[survey_dt_new$Address2 == "일산서" & survey_dt_new$Address1 == "고양시"  ,][, c("Address2")] = "고양시 일산서구" # 일산서구도 있음
survey_dt_new[survey_dt_new$Address2 == "독양구" & survey_dt_new$Address1 == "고양시"  ,][, c("Address2")] = "고양시 덕양구" # 오타
survey_dt_new[survey_dt_new$Address2 == "원신동" & survey_dt_new$Address1 == "고양시"  ,][, c("Address1", "Address2")] = c("고양시 덕양구", "원당동") # 일산서구도 있음




survey_dt_new[survey_dt_new$Address2 == "화정동" & survey_dt_new$Address1 == "고양시"  ,][, c("Address1")] = "고양시 덕양구"
survey_dt_new[survey_dt_new$Address2 == "금곡동" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1")] = "성남시 분당구"
survey_dt_new[survey_dt_new$Address2 == "대장동" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1")] = "성남시 분당구"
survey_dt_new[survey_dt_new$Address2 == "금곡동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1")] = "수원시 권선구"
survey_dt_new[survey_dt_new$Address2 == "탑동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1")] = "수원시 권선구"

survey_dt_new[survey_dt_new$Address2 == "장안동" & survey_dt_new$Address1 == "수원시"  ,][, c("Address1")] = "수원시 팔달구"



survey_dt_new[survey_dt_new$Address2 == "중동" & survey_dt_new$Address1 == "용인시"  ,][, c("Address1")] = "용인시 기흥구"
survey_dt_new[survey_dt_new$Address2 == "중앙동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address1")] = "창원시 진해구" # 성산구도 있음 

survey_dt_new[survey_dt_new$Address2 == "상남동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address1")] = "창원시마산합포구" # 성산구도 있음 

survey_dt_new[survey_dt_new$Address2 == "남양동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address1")] = "창원시 진해구" # 성산구도 있음 

survey_dt_new[survey_dt_new$Address2 == "평안동" & survey_dt_new$Address1 == "안양시"  ,][, c("Address1", "Address2")] = c("안양시 동안구", "평촌동") # 성산구도 있음 


survey_dt_new[survey_dt_new$Address2 == "대방동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address1")] = "창원시 성산구" # 
survey_dt_new[survey_dt_new$Address2 == "내동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address1")] = "창원시 성산구" # 

survey_dt_new[survey_dt_new$Address2 == "북부동" & survey_dt_new$Address1 == "김해시"  ,][, c("Address2")] = "삼계동" # 행정동. 구산동, 대성동도 있음
survey_dt_new[survey_dt_new$Address2 == "동부동" & survey_dt_new$Address1 == "경산시"  ,][, c("Address2")] = "남방동" # 행정동. 다른 동도 있음.
survey_dt_new[survey_dt_new$Address2 == "중앙동" & survey_dt_new$Address1 == "평택시"  ,][, c("Address2")] = "장당동" # 행정동. 다른 동도 있음.

survey_dt_new[survey_dt_new$Address2 == "신사동" & survey_dt_new$Address1 == "관악구"  ,][, c("Address2")] = "신림동" # 행정동. 


survey_dt_new[survey_dt_new$Address2 == "서대신동" & survey_dt_new$Province == "부산"  ,][, c("Address2")] = "서대신동1가" # 행정동. 
survey_dt_new[survey_dt_new$Address2 == "토성동" & survey_dt_new$Province == "부산"  ,][, c("Address2")] = "토성동1가" # 

survey_dt_new[survey_dt_new$Address2 == "치평" & survey_dt_new$Province == "광주"  ,][, c("Address2")] = "치평동" # 
survey_dt_new[survey_dt_new$Address2 == "유성" & survey_dt_new$Province == "대전"  ,][, c("Address2")] = "유성구" # 
survey_dt_new[survey_dt_new$Address2 == "듀산" & survey_dt_new$Province == "대전"  ,][, c("Address2")] = "유성구" # 

survey_dt_new[survey_dt_new$Address2 == "온천동" & survey_dt_new$Address1 == "유성구"  ,][, c("Address2")] = "봉명동" #행정동. 다른 동도 있음 


survey_dt_new[survey_dt_new$Address2 == "인동" & survey_dt_new$Address1 == "구미시"  ,][, c("Address2")] = "신동" # 인동동 행정동. 
survey_dt_new[survey_dt_new$Address2 == "창녕면" & survey_dt_new$Address1 == "창녕군"  ,][, c("Address2")] = "창녕읍"

survey_dt_new[survey_dt_new$Address2 == "진량면" & survey_dt_new$Address1 == "경산시"  ,][, c("Address2")] = "진량읍"
survey_dt_new[survey_dt_new$Address2 == "압량면" & survey_dt_new$Address1 == "경산시"  ,][, c("Address2")] = "압량읍"

survey_dt_new[survey_dt_new$Address2 == "당진읍" & survey_dt_new$Address1 == "당진시"  ,][, c("Address2")] = "당진시"


survey_dt_new[survey_dt_new$Address2 == "명륜로" & survey_dt_new$Address1 == "동래구"  ,][, c("Address2")] = "명륜동" #
survey_dt_new[survey_dt_new$Address2 == "가수원동" & survey_dt_new$Address1 == "서구"  ,][, c("Address2")] = "" # 없는 지명
survey_dt_new[survey_dt_new$Address2 == "연수로" & survey_dt_new$Address1 == "수영구"  ,][, c("Address2")] = "수영동" # 도로명
survey_dt_new[survey_dt_new$Address2 == "영선동" & survey_dt_new$Address1 == "영도구"  ,][, c("Address2")] = "영선동1가" #
survey_dt_new[survey_dt_new$Address2 == "천로길" & survey_dt_new$Address1 == "영도구"  ,][, c("Address2")] = "동삼동" # 해수천로

survey_dt_new[survey_dt_new$Address2 == "봉독동" & survey_dt_new$Address1 == "남구"  ,][, c("Address2")] = "봉덕동" # 오타

survey_dt_new[survey_dt_new$Address2 == "칠성동" & survey_dt_new$Address1 == "달서구"  ,][, c("Address2")] = "칠성동1가" # 오타

survey_dt_new[survey_dt_new$Address2 == "성내동" & survey_dt_new$Address1 == "중구"  ,][, c("Address2")] = "봉산동" # 행정동. 다른 동도 있음
survey_dt_new[survey_dt_new$Address2 == "연기동" & survey_dt_new$Address1 == "연기면"  ,][, c("Address2")] = "연기리" # 
survey_dt_new[survey_dt_new$Address2 == "도계동" & survey_dt_new$Address1 == "삼척시"  ,][, c("Address2")] = "도계읍" # 
survey_dt_new[survey_dt_new$Address2 == "철전동" & survey_dt_new$Address1 == "춘천시"  ,][, c("Address2")] = "칠전동" #오타 

survey_dt_new[survey_dt_new$Address2 == "수성" & survey_dt_new$Address1 == "달성군"  ,][, c("Address1", "Address2")] =c("수성구", "수성구") #오타 


survey_dt_new[survey_dt_new$Address2 == "종로" & survey_dt_new$Address1 == "종로구"  ,][, c("Address2")] = "종로구" # 
survey_dt_new[survey_dt_new$Address2 == "명륜동" & survey_dt_new$Address1 == "종로구"  ,][, c("Address2")] = "명륜1가" # 2, 3가도 있음 
survey_dt_new[survey_dt_new$Address2 == "신문로동" & survey_dt_new$Address1 == "종로구"  ,][, c("Address2")] = "신문로1가" #  2가도 있음
survey_dt_new[survey_dt_new$Address2 == "인이동" & survey_dt_new$Address1 == "종로구"  ,][, c("Address2")] = "인의동" # 

survey_dt_new[survey_dt_new$Address2 == "청룡동" & survey_dt_new$Address1 == "천안시"  ,][c("Address1", "Address2")] = c("천안시 동남구", "구성동") # 행정동. 다른 동도 있음 


# survey_dt_new[survey_dt_new$Address2 == "여후동" & survey_dt_new$Address1 == "옹진군"  ,][, c("Address2")] = "영흥면" 


survey_dt_new[survey_dt_new$Address2 == "상평" & survey_dt_new$Address1 == "부평구"  ,][, c("Address2")] = "" # 없는 지명 

survey_dt_new[survey_dt_new$Address2 == "병영동" & survey_dt_new$Address1 == "중구"  ,][, c("Address2")] = "남외동" # 행정동 다른 동도 있음

survey_dt_new[survey_dt_new$Address2 == "문덕리" & survey_dt_new$Address1 == "포항시"  ,][, c("Address1")] = "포항시남구" 

survey_dt_new[survey_dt_new$Address2 == "장량동" & survey_dt_new$Address1 == "포항시"  ,][c("Address1", "Address2")] = c("포항시 북구", "장성동") # 행정동. 다른 동도 있음 
survey_dt_new[survey_dt_new$Address2 == "상대동" & survey_dt_new$Address1 == "포항시"  ,][c("Address1", "Address2")] = c("포항시남구", "상도동") # 행정동. 다른 동도 있음 

survey_dt_new[survey_dt_new$Address2 == "대이동" & survey_dt_new$Address1 == "포항시"  ,][c("Address1", "Address2")] = c("포항시남구", "대잠동") # 행정동. 다른 동도 있음 
survey_dt_new[survey_dt_new$Address2 == "우창동" & survey_dt_new$Address1 == "포항시"  ,][c("Address1", "Address2")] = c("포항시 북구", "학산동") # 행정동. 다른 동도 있음 
survey_dt_new[survey_dt_new$Address2 == "효곡동" & survey_dt_new$Address1 == "포항시"  ,][c("Address1", "Address2")] = c("포항시남구", "효자동") # 행정동. 다른 동도 있음 





survey_dt_new[survey_dt_new$Address2 == "쌍용동" & survey_dt_new$Address1 == "천안시"  ,][, c("Address1")] = "천안시 동남구" # 서북구도 있음 

survey_dt_new[survey_dt_new$Address2 == "은행동" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1")] = "시흥시" # 

survey_dt_new[survey_dt_new$Address2 == "이동" & survey_dt_new$Address1 == "안산시"  ,][, c("Address1")] = "안산시 상록구" # 
survey_dt_new[survey_dt_new$Address2 == "이동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address1")] = "창원시 진해구" # 

survey_dt_new[survey_dt_new$Address2 == "신월동" & survey_dt_new$Address1 == "창원시"  ,][, c("Address1")] = "창원시 성산구" # 마산합포구도 있음



survey_dt_new[survey_dt_new$Address2 == "은행동" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1")] = "성남시 중원구" 
survey_dt_new[survey_dt_new$Address2 == "정자동" & survey_dt_new$Address1 == "성남시"  ,][, c("Address1")] = "성남시 분당구" 


survey_dt_new[survey_dt_new$Address2 == "부곡동" & survey_dt_new$Address1 == "의왕시"  ,][, c("Address1", "Address2")] = c("의왕시", "이동") # 행정동. 다른 동도 있음 


survey_dt_new[survey_dt_new$Address2 == "방화종" ,]$Address2 = "방화동" # 
survey_dt_new[survey_dt_new$Address2 == "반야월북로" ,]$Address2 = "신서동" # 대구  
survey_dt_new[survey_dt_new$Address2 == "신쳔동" ,]$Address2 = "신천동" # 대구


survey_dt_new[survey_dt_new$Address2 == "허준로" ,]$Address2 = "가양동" # 
survey_dt_new[survey_dt_new$Address2 == "노원동" ,]$Address2 = "" # 노원구
survey_dt_new[survey_dt_new$Address2 == "쌍문" ,]$Address2 = "쌍문동" # 


survey_dt_new[survey_dt_new$Address2 == "이운동" ,]$Address2 = "이문동" # 
survey_dt_new[survey_dt_new$Address2 == "봉래동" ,]$Address2 = "봉래동1가" # 
survey_dt_new[survey_dt_new$Address2 == "유" ,]$Address2 = "유동" # 광주 유동 


# survey_dt_new[survey_dt_new$Address2 == "동대신동" ,]$Address2 = "동대신동1가" # 



typo_A2_v1 = c("강남일원", "강낭동", "강남", "강북", "광진", "동대문", "마포", "성동", "금정", "동", "미추홀", "ㅁㄱㄴ", "ㄴ", "강동", "성수", "서울", "ㄷㄹ", "ㅎ", "읍내", "당", "ㅇㅇ동", "집읍")
survey_dt_new[survey_dt_new$Address2 %in% typo_A2_v1,]$Address2 = "" 


survey_dt_new$Address1 = str_trim(survey_dt_new$Address1)
survey_dt_new$Address2 = str_trim(survey_dt_new$Address2)
survey_dt_new$Address3 = str_trim(survey_dt_new$Address3)


nrow(survey_dt_new)

# # Based on Address1
# survey_dt_new$Address12 = survey_dt_new$Address1
# # if A1 empty but A2 avail
# A1NA_A2NONA_IDX = is.na(survey_dt_new$Address1 ) & !is.na(survey_dt_new$Address2)
# survey_dt_new$Address12[A1NA_A2NONA_IDX] = survey_dt_new[A1NA_A2NONA_IDX,]$Address2
# survey_dt_new$Address12[A1NA_A2NONA_IDX] = survey_dt_new[A1NA_A2NONA_IDX,]$Address2
# 
# # Only A3 avail?
# A1NA_A2NA_A3NONA_IDX = is.na(survey_dt_new$Address1 ) & is.na(survey_dt_new$Address2) & !is.na(survey_dt_new$Address3 ) # there's no case like that
# table(A1NA_A2NA_A3NONA_IDX) 



# df %>% distinct(id, id2, .keep_all = TRUE)
# df %>% group_by(id, id2) %>% filter(row_number() == 1)
# df %>% group_by(id, id2) %>% slice(1)

admincode_dt_reduced = admincode_dt[, ] %>% distinct(BJD_Code, .keep_all =TRUE)
nrow(admincode_dt_reduced)
length(unique(admincode_dt_reduced$BJD_Code))

c("Regioncode", "BJD", "BJD_Code")


# STEP1: Use Add1 and Add2

survey_dt_new_merged1 = merge(y = data.frame(survey_dt_new[,]), x=data.frame(admincode_dt_reduced)[,c("RegionCode", "SGG", "BJD", "BJD_Code", "HJD_Kor")],  by.y = c("RegionCode", "Address1", "Address2"), by.x = c("RegionCode", "SGG", "BJD"), all.x=F, all.y = T)


survey_dt_new_merged1[survey_dt_new_merged1$ObsID == 9701,]$Address1
survey_dt_new_merged1[survey_dt_new_merged1$ObsID == 9701,]$Address2



head(survey_dt_new_merged1[,])

nrow(survey_dt_new_merged1) # 중복된 법정동 이름 있음
# 예를 들어,  
survey_dt_new_merged1[survey_dt_new_merged1$ObsID == "408",] # 포승읍과 팽성읍

# 첫 번째만 택할 것

survey_dt_new_merged1 = survey_dt_new_merged1 %>% distinct(ObsID, .keep_all = TRUE)

stopifnot(nrow(survey_dt_new_merged1) == nrow(survey_dt_new))

# colnames(survey_dt_new_merged1)
# colnames(survey_dt_new)
table(is.na(survey_dt_new_merged1$BJD_Code))

# Re-create Add1 and Add2
survey_dt_new_merged1$Address1 = survey_dt_new_merged1$SGG
survey_dt_new_merged1$Address2 = survey_dt_new_merged1$BJD

# Reshape the data frame
survey_dt_new_merged1 = survey_dt_new_merged1[, c(colnames(survey_dt_new), "BJD", "BJD_Code")]

# not matched 
survey_dt_new_merged1_na = survey_dt_new_merged1[is.na(survey_dt_new_merged1$BJD_Code),]
nrow(survey_dt_new_merged1_na)
# head(survey_dt_new_merged1_na)



# Drop the NA values
survey_dt_new_merged1_na$BJD_Code = NULL
survey_dt_new_merged1_na$BJD = NULL

# STEP2 : Use Add1 + Add2

# For e.g., 마산 합포구
survey_dt_new_merged1_na$Add1plusAdd2 = paste0(survey_dt_new_merged1_na$Address1, survey_dt_new_merged1_na$Address2) 

survey_dt_new_merged1_na$Add1plusAdd2 = stringr::str_remove(survey_dt_new_merged1_na$Add1plusAdd2, pattern = " ")


admincode_dt_reduced$SGG_collapsed = stringr::str_remove(admincode_dt_reduced$SGG, pattern = " ")


survey_dt_new_merged2 = merge(y = data.frame(survey_dt_new_merged1_na[,]), x=data.frame(admincode_dt_reduced)[,c("RegionCode", "SGG_collapsed", "BJD", "BJD_Code")],  by.y = c("RegionCode", "Add1plusAdd2", "Address3"), by.x = c("RegionCode", "SGG_collapsed", "BJD"), all.x=F, all.y = T)
stopifnot(nrow(survey_dt_new_merged2) == nrow(survey_dt_new_merged1_na))

nrow(survey_dt_new_merged2[!is.na(survey_dt_new_merged2$BJD_Code), ]) # 116 matches

# Re-create Add1 and Add2
survey_dt_new_merged2$Address3 = survey_dt_new_merged2$BJD

# Reshape the data frame
survey_dt_new_merged2 = survey_dt_new_merged2[, c(colnames(survey_dt_new), "BJD", "BJD_Code")]

# not matched 
survey_dt_new_merged2_na = survey_dt_new_merged2[is.na(survey_dt_new_merged2$BJD_Code),]
nrow(survey_dt_new_merged2_na)
# head(survey_dt_new_merged1_na)

# Drop the NA values
survey_dt_new_merged2_na$BJD_Code = NULL
survey_dt_new_merged2_na$BJD = NULL



head(survey_dt_new_merged2_na)



# STEP 3: Use only Add2
survey_dt_new_merged3 = merge(x=data.frame(admincode_dt_reduced)[,], y = data.frame(survey_dt_new_merged2_na[,]),  by.x = c("RegionCode", "BJD"), by.y = c("RegionCode", "Address2"),  all.x=F, all.y = T)
nrow(survey_dt_new_merged3)


# tb1 = table(survey_dt_new_merged3$ObsID)
# tb2 = tb1[tb1 >= 2 ]
# tb2
#  중복 있었으나 위에서 해결

nrow(distinct(survey_dt_new_merged3, ObsID))


tb1 = table(survey_dt_new_merged3$ObsID) 
tb1[tb1>1]

survey_dt_new_merged2_na[survey_dt_new_merged2_na$ObsID == 5096,] # 세종시가 코드가 2개임
survey_dt_new_merged3[survey_dt_new_merged3$ObsID == 5096,]

# 중복 있음
# stopifnot(nrow(survey_dt_new_merged3) == nrow(survey_dt_new_merged2_na))

survey_dt_new_merged3 = distinct(survey_dt_new_merged3, ObsID, .keep_all = T)

nrow(survey_dt_new_merged3[!is.na(survey_dt_new_merged3$BJD_Code), ]) # 865 matches

stopifnot(nrow(survey_dt_new_merged3) == nrow(survey_dt_new_merged2_na))


# Re-create Add2
survey_dt_new_merged3$Address2 = survey_dt_new_merged3$BJD

# Reshape the data frame
survey_dt_new_merged3 = survey_dt_new_merged3[, c(colnames(survey_dt_new), "BJD", "BJD_Code")]


# not matched 
survey_dt_new_merged3_na = survey_dt_new_merged3[is.na(survey_dt_new_merged3$BJD_Code),]


nrow(survey_dt_new_merged3_na) # 2488

# Drop the NA values
survey_dt_new_merged3_na$BJD_Code = NULL
survey_dt_new_merged3_na$BJD = NULL








# STEP4 : Use Add1 + Add2 w/o Add3

# For e.g., 마산 합포구
survey_dt_new_merged3_na$Add1plusAdd2 = paste0(survey_dt_new_merged3_na$Address1, survey_dt_new_merged3_na$Address2) 

survey_dt_new_merged3_na$Add1plusAdd2 = stringr::str_remove(survey_dt_new_merged3_na$Add1plusAdd2, pattern = " ")

# 시군구의 법정동 코드만 남김
admincode_dt_reduced_SGG = admincode_dt_reduced[admincode_dt_reduced$HJD_Kor  == admincode_dt_reduced$SGG, ]


survey_dt_new_merged4 = merge(y = data.frame(survey_dt_new_merged3_na[,]), x=data.frame(admincode_dt_reduced_SGG)[,c("RegionCode", "SGG_collapsed", "BJD", "BJD_Code")],  by.y = c("RegionCode", "Add1plusAdd2"), by.x = c("RegionCode", "SGG_collapsed"), all.x=F, all.y = T)




tb1 = table(survey_dt_new_merged4$ObsID) 
tb1[tb1>1]
survey_dt_new_merged4[survey_dt_new_merged4$ObsID == 10158,] # 세종시가 코드가 2개임
survey_dt_new_merged3_na[survey_dt_new_merged3_na$ObsID == 10158,]




stopifnot(nrow(survey_dt_new_merged4) == nrow(survey_dt_new_merged3_na))

nrow(survey_dt_new_merged4[!is.na(survey_dt_new_merged4$BJD_Code), ]) # 67 matches


# Reshape the data frame
survey_dt_new_merged4 = survey_dt_new_merged4[, c(colnames(survey_dt_new), "BJD", "BJD_Code")]

# not matched 
survey_dt_new_merged4_na = survey_dt_new_merged4[is.na(survey_dt_new_merged4$BJD_Code),]
nrow(survey_dt_new_merged4_na)
# head(survey_dt_new_merged1_na)

# Drop the NA values
survey_dt_new_merged4_na$BJD_Code = NULL
survey_dt_new_merged4_na$BJD = NULL



table(survey_dt_new_merged4_na$Province)







### STEP5 Use HJD



survey_dt_new_merged5= merge(y = data.frame(survey_dt_new_merged4_na[,]), x=data.frame(admincode_dt)[,c("RegionCode", "SGG", "HJD_Kor", "BJD", "BJD_Code")],  by.y = c("RegionCode", "Address1", "Address2"), by.x = c("RegionCode", "SGG", "HJD_Kor"), all.x=F, all.y = T)

nrow(survey_dt_new_merged5[!is.na(survey_dt_new_merged5$BJD_Code), ]) # 810 matches 행정동에 법정동 여러개라서

# drop the redundant.. 
survey_dt_new_merged5 = distinct(survey_dt_new_merged5, ObsID, .keep_all = T)

stopifnot(nrow(survey_dt_new_merged5) == nrow(survey_dt_new_merged4_na))


# Re-create Add1 and Add2
survey_dt_new_merged5$Address1 = survey_dt_new_merged5$SGG
survey_dt_new_merged5$Address2 = survey_dt_new_merged5$HJD_Kor


# Reshape the data frame
survey_dt_new_merged5 = survey_dt_new_merged5[, c(colnames(survey_dt_new), "BJD", "BJD_Code")]

# not matched 
survey_dt_new_merged5_na = survey_dt_new_merged5[is.na(survey_dt_new_merged5$BJD_Code),]
nrow(survey_dt_new_merged5_na)
# head(survey_dt_new_merged1_na)

# Drop the NA values
survey_dt_new_merged5_na$BJD_Code = NULL
survey_dt_new_merged5_na$BJD = NULL




# survey_dt_new_merged5_na[survey_dt_new_merged5_na$Address2 == "성수동",]
# 
# tb1 = table(survey_dt_new_merged5_na$Address2) %>% sort 
# 
# tb1[tb1 > 1 ]
# 
# table(survey_dt_new_merged5_na[survey_dt_new_merged5_na$Address1=="전주시", ]$Address2)
# table(survey_dt_new_merged5_na[survey_dt_new_merged5_na$Address1=="서구", ]$Address2)
# 
# (survey_dt_new_merged5_na[survey_dt_new_merged5_na$Address2=="평안동", ])
# (survey_dt_new_merged5_na[survey_dt_new_merged5_na$Address2=="장량동", ])
# 
# 
# 
tb3 = table(survey_dt_new_merged5_na$Province) %>% sort %>% print
tb3



### STEP6 Use only Add1 when ADD2 is empty


survey_dt_new_merged6_na_ADD2missing = survey_dt_new_merged5_na[is.na(survey_dt_new_merged5_na$Address2) | survey_dt_new_merged5_na$Address2 == "", ]
nrow(survey_dt_new_merged6_na_ADD2missing)

survey_dt_new_merged6 = merge(x=data.frame(admincode_dt_reduced)[,], y = data.frame(survey_dt_new_merged6_na_ADD2missing[,]),  by.x = c("RegionCode", "BJD"), by.y = c("RegionCode", "Address1"),  all.x=F, all.y = T)
nrow(survey_dt_new_merged6)
nrow(survey_dt_new_merged6[!is.na(survey_dt_new_merged6$BJD_Code), ]) #  


# 신흥리가 조치원읍과 전의면 둘 있음
survey_dt_new_merged6 = distinct(survey_dt_new_merged6, ObsID, .keep_all = T)


stopifnot(nrow(survey_dt_new_merged6_na_ADD2missing) == nrow(survey_dt_new_merged6))



# Re-create Add1 and Add2
survey_dt_new_merged6$Address1 = survey_dt_new_merged6$BJD

# Reshape the data frame
survey_dt_new_merged6 = survey_dt_new_merged6[, c(colnames(survey_dt_new), "BJD", "BJD_Code")]

# not matched 
survey_dt_new_merged6_na = survey_dt_new_merged6[is.na(survey_dt_new_merged6$BJD_Code),]
stopifnot(nrow(survey_dt_new_merged6_na)==0)
# head(survey_dt_new_merged1_na)

# # Drop the NA values
survey_dt_new_merged6_na$BJD_Code = NULL
survey_dt_new_merged6_na$BJD = NULL


survey_dt_new_merged7_na = survey_dt_new_merged5_na[!(survey_dt_new_merged5_na$ObsID %in% survey_dt_new_merged6$ObsID),]

### STEP8 add 'Dong'

survey_dt_new_merged7_na$Address2Dong = paste0(survey_dt_new_merged7_na$Address2, "동")

survey_dt_new_merged8 = merge(x=data.frame(admincode_dt_reduced)[,], y = data.frame(survey_dt_new_merged7_na[,]),  by.x = c("RegionCode", "SGG", "BJD"), by.y = c("RegionCode", "Address1", "Address2Dong"),  all.x=F, all.y = T)
nrow(survey_dt_new_merged8)
nrow(survey_dt_new_merged8[!is.na(survey_dt_new_merged8$BJD_Code), ]) #  6 matches

stopifnot(nrow(survey_dt_new_merged7_na) == nrow(survey_dt_new_merged8))


# Re-create Add1 and Add2
survey_dt_new_merged8$Address1 = survey_dt_new_merged8$SGG

# Reshape the data frame
survey_dt_new_merged8 = survey_dt_new_merged8[, c(colnames(survey_dt_new), "BJD", "BJD_Code")]

# not matched 
survey_dt_new_merged8_na = survey_dt_new_merged8[is.na(survey_dt_new_merged8$BJD_Code),]

# # Drop the NA values
survey_dt_new_merged8_na$BJD_Code = NULL
survey_dt_new_merged8_na$BJD = NULL





nrow(survey_dt_new_merged8_na)

# survey_dt_new_merged8_na[,5:7]
# 
# 
#  
# 
# tb3 = table(survey_dt_new_merged8_na$Address1) %>% sort %>% print



### Data set merging

survey_dt_new_merged1_nona = survey_dt_new_merged1[!is.na(survey_dt_new_merged1$BJD_Code), ]
survey_dt_new_merged2_nona = survey_dt_new_merged2[!is.na(survey_dt_new_merged2$BJD_Code), ]
survey_dt_new_merged3_nona = survey_dt_new_merged3[!is.na(survey_dt_new_merged3$BJD_Code), ]
survey_dt_new_merged4_nona = survey_dt_new_merged4[!is.na(survey_dt_new_merged4$BJD_Code), ]
survey_dt_new_merged5_nona = survey_dt_new_merged5[!is.na(survey_dt_new_merged5$BJD_Code), ]
survey_dt_new_merged6_nona = survey_dt_new_merged6[!is.na(survey_dt_new_merged6$BJD_Code), ]
survey_dt_new_merged8_nona = survey_dt_new_merged8[!is.na(survey_dt_new_merged8$BJD_Code), ]
 
survey_dt_new_merged_l = list(survey_dt_new_merged1_nona, survey_dt_new_merged2_nona, survey_dt_new_merged3_nona, survey_dt_new_merged4_nona, survey_dt_new_merged5_nona, survey_dt_new_merged6_nona, survey_dt_new_merged8_nona)

sum(sapply(survey_dt_new_merged_l, FUN = nrow))
nrow(survey_dt_new)




# 2022 and 2023
sapply(survey_dt_new_merged_l, FUN = function(x) table(x$Year))




survey_dt_new_merged_df = do.call("rbind", survey_dt_new_merged_l)

tb1 = table(survey_dt_new_merged_df$ObsID)
tb1[tb1>1]

sapply(survey_dt_new_merged_l, FUN = function(x) table(x$ObsID == 4123))


survey_dt_new_merged_df[survey_dt_new_merged_df$ObsID == 4123,]

nrow(distinct(survey_dt_new_merged_df, ObsID))


all(survey_dt_new$ObsID %in% survey_dt_new_merged_df$ObsID )

# reshape 
survey_dt_new_merged_df = survey_dt_new_merged_df[, colnames(survey_dt_new_merged_df)[c(1:5, 61:63, 6:60) ]]

write.xlsx(survey_dt_new_merged_df, file = paste0(path_out, "survey_dt_new_merged_", Sys.Date(), ".xlsx"))

stop("ends here")



write.xlsx(survey_dt_new_merged1, file = paste0(path_out, "survey_dt_new_merged1_", Sys.Date(), ".xlsx"))
write.xlsx(survey_dt_new_merged1_na, file = paste0(path_out, "survey_dt_new_merged1_na_", Sys.Date(), ".xlsx"))

write.xlsx(survey_dt_new_merged2, file = paste0(path_out, "survey_dt_new_merged2_", Sys.Date(), ".xlsx"))
write.xlsx(survey_dt_new_merged2_na, file = paste0(path_out, "survey_dt_new_merged2_na_", Sys.Date(), ".xlsx"))




write.xlsx(survey_dt_new_merged3, file = paste0(path_out, "survey_dt_new_merged3_", Sys.Date(), ".xlsx"))
write.xlsx(survey_dt_new_merged3_na, file = paste0(path_out, "survey_dt_new_merged3_na_", Sys.Date(), ".xlsx"))

write.xlsx(survey_dt_new_merged4, file = paste0(path_out, "survey_dt_new_merged4_", Sys.Date(), ".xlsx"))
write.xlsx(survey_dt_new_merged4_na, file = paste0(path_out, "survey_dt_new_merged4_na_", Sys.Date(), ".xlsx"))


write.xlsx(survey_dt_new_merged5, file = paste0(path_out, "survey_dt_new_merged5_", Sys.Date(), ".xlsx"))
write.xlsx(survey_dt_new_merged5_na, file = paste0(path_out, "survey_dt_new_merged5_na_", Sys.Date(), ".xlsx"))


write.xlsx(survey_dt_new_merged6, file = paste0(path_out, "survey_dt_new_merged6_", Sys.Date(), ".xlsx"))
write.xlsx(survey_dt_new_merged7_na, file = paste0(path_out, "survey_dt_new_merged7_na_", Sys.Date(), ".xlsx"))
write.xlsx(survey_dt_new_merged8, file = paste0(path_out, "survey_dt_new_merged8_na_", Sys.Date(), ".xlsx"))










nrow(survey_dt_new_merged3[!is.na(survey_dt_new_merged3$BJD_Code), ]) # 865 matches



# Re-create Add1 and Add2
survey_dt_new_merged3$Address2 = survey_dt_new_merged3$BJD

# Reshape the data frame
survey_dt_new_merged3 = survey_dt_new_merged3[, c(colnames(survey_dt_new), "BJD", "BJD_Code")]


# not matched 
survey_dt_new_merged3_na = survey_dt_new_merged3[is.na(survey_dt_new_merged3$BJD_Code),]


nrow(survey_dt_new_merged3_na) # 2488

# Drop the NA values
survey_dt_new_merged3_na$BJD_Code = NULL
survey_dt_new_merged3_na$BJD = NULL





# STEP4: use HJD
survey_dt_new_merged4 = merge( y = data.frame(survey_dt_new_merged3_na[,]), x=data.frame(admincode_dt)[,],  by.y = c("RegionCode", "Address1", "Address2"), by.x = c("RegionCode", "SGG", "HJD_Kor"), all.x=F, all.y = T)

nrow(distinct(survey_dt_new_merged4, ObsID))

nrow(survey_dt_new_merged4)
nrow(survey_dt_new_merged4[!is.na(survey_dt_new_merged4$BJD_Code), ]) # 810 matches

# 중복 있음


survey_dt_new_merged3[is.na(survey_dt_new_merged3$BJD_Code.y),]


survey_dt_new_merged3_na = survey_dt_new_merged3[is.na(survey_dt_new_merged3$BJD_Code.y),]

nrow(survey_dt_new_merged3_na)






stopifnot(nrow(survey_dt_new_merged1)  == nrow(survey_dt))




length(unique(survey_dt_new_merged1$ObsID))
tb1 = table(survey_dt_new_merged1$ObsID)

names(tb1[tb1 > 1])

survey_dt_new_merged1[survey_dt_new_merged1$ObsID == "408",]



survey_dt_new_merged1

table(!is.na(survey_dt_new_merged1$BJD_Code))



table(!is.na(survey_dt_new_merged2$BJD_Code))

head(survey_dt_new_merged2)

survey_dt_new_merged2_na = survey_dt_new_merged2[is.na(survey_dt_new_merged2$BJD_Code),]

head(survey_dt_new_merged2_na)




# table(survey_dt_new$Address1 %in% sgg_bjd_df$시군구)
# table(survey_dt_new$Address1[! (survey_dt_new$Address1 %in% sgg_bjd_df$시군구)])



# names(table(survey_dt_new$Address12[! (survey_dt_new$Address12 %in% sgg_bjd_df$SGG_BJD)]))








survey_dt_new_merged1_na = survey_dt_new_merged1[is.na(survey_dt_new_merged1$BJD_Code),]


table(is.na(survey_dt_new_merged1_na$Address3))






survey_dt_new_merged2 = merge(x=admincode_dt[,c("RegionCode", "BJD", "BJD_Code")], y = data.frame(survey_dt_new_merged1_na[,]),  by.x = c("RegionCode", "BJD"), by.y = c("RegionCode", "Address2"), all.x=F, all.y = T)



tail(survey_dt_new_merged1[is.na(survey_dt_new_merged1$BJD_Code), ])
head(survey_dt_new_merged1[is.na(survey_dt_new_merged1$BJD_Code), ])


# admincode_dt[admincode_dt$BJD == "야음동",]






table(sapply(survey_dt_new_merged$TX3_3_RE, FUN = function(x) strsplit(x, " ")[[1]] %>% length))


table(is.na(survey_dt_new_merged$SGG_CODE))
table(survey_dt_new_merged$SGG_CODE=="")

survey_dt_new_merged_error = survey_dt_new_merged[(survey_dt_new_merged$SGG_CODE =="") | (survey_dt_new_merged$SGG_CODE %>% is.na),]


nrow(survey_dt_new_merged_error)

# a =table(survey_dt_new_merged_error$TX3_3_RE)
# a
# names(a)
survey_dt_new_merged_error$TX3_3_RE
# 26 


write.xlsx(survey_dt_new_merged_error, file = paste0(path_out, "survey_dt_new_nodata", Sys.Date(), ".xlsx"))



table(is.na(survey_dt_new_merged$SGG_CODE))


table(survey_dt_new_merged$SGG_CODE %in% sgg_shp$SIG_CD)


table(survey_dt_new_merged$SGG [!(survey_dt_new_merged$SGG_Code %in% sgg_shp$SIG_CD)])


# (survey_dt_new_merged[!(survey_dt_new_merged$SGG_Code %in% sgg2_shp$SIGUNGU_CD), ])
table(survey_dt_new_merged[!(survey_dt_new_merged$SGG_CODE %in% sgg_shp$SIG_CD), ]$TX3_3_RE)




write.xlsx(survey_dt_new_merged, file = paste0(path_out, "survey_dt_new_", Sys.Date(), ".xlsx"))



library(sf)

# Spatial merging
q_idx = 1 
col_cnt_v = paste0("Q", 15:35)


res_dt_l = foreach (q_idx = seq_along(col_cnt_v)) %do% { 
    col_nm_tmp = col_cnt_v[q_idx] 
    
    
    # cat
    
    # res_tmp = tapply(survey_dt_new_merged[, col_nm_tmp], INDEX = survey_dt_new_merged$SGG_CODE, FUN = function(x) {r1 = table(factor(x, 1:5)); return(r1/sum(r1, na.rm=T))})
    
    # cont. 
    res_tmp = tapply(survey_dt_new_merged[, col_nm_tmp], INDEX = survey_dt_new_merged$SGG_CODE, FUN = function(x) {c(mean(x, na.rm=T), sd(x, na.rm=T))})
    
    
    res_df = do.call(rbind, res_tmp) %>% data.frame
    res_df = cbind(rownames(res_df), res_df)
    colnames(res_df) = c("SGG_CODE", "Mean", "SD")
    
    
    dt1 = merge(sgg_shp@data, y = res_df, by.x = "SIG_CD", by.y = "SGG_CODE", all.x = TRUE)
    
    dt1 = dt1[,4:5]
    colnames(dt1) = paste0(col_nm_tmp,"_", c("Avg", "SD"))
    
    return(dt1)
}

res_dt_df = do.call("cbind", res_dt_l)

res_shp = sgg_shp

res_shp@data = cbind(sgg_shp@data, res_dt_df)

writeOGR(res_shp, paste0(path_out), layer = paste0("Survey_AvgSD_bySGG"), driver = "ESRI Shapefile", encoding = "EUC-KR", overwrite_layer = T)


stop("ends here")







r_idx = 1  









for (r_idx in seq_along(RegionCode_new)) { 
    
    r_code= RegionCode_new[r_idx]
    
    survey_dt_tmp = survey_dt[survey_dt$Q1_new == r_code,]
    admincode_dt_tmp = admincode_dt[admincode_dt$RegionCode == r_code,]
    
    table(survey_dt_tmp$TX3_2 %in% admincode_dt_tmp$HJD_Kor)
    table(survey_dt_tmp$TX3_2 %in% admincode_dt_tmp$HJD_2nd)
    table(survey_dt_tmp$TX3_2 %in% admincode_dt_tmp$BJD)
    
    
    
    
    BJD_Y_idx = survey_dt_tmp$TX3_2 %in% admincode_dt_tmp$BJD
    HJD_Y_idx = survey_dt_tmp$TX3_2 %in% admincode_dt_tmp$HJD_Kor
    HJD2_Y_idx = survey_dt_tmp$TX3_2 %in% admincode_dt_tmp$HJD_2nd
    
    BJD_N_HJD_Y_idx = (!BJD_Y_idx) & HJD_Y_idx
    BJD_N_HJD2_Y_idx = (!BJD_Y_idx) &  (!HJD_Y_idx) & HJD2_Y_idx
    
    
    
    
    
    
    survey_dt_tmp$BJD = NA
    # BJD
    survey_dt_tmp$BJD[BJD_Y_idx] = survey_dt_tmp$TX3_2[BJD_Y_idx]
    # BJD found by HJD
    survey_dt_tmp$BJD[BJD_N_HJD_Y_idx] = admincode_dt_tmp$BJD[match(survey_dt_tmp$TX3_2[BJD_N_HJD_Y_idx], admincode_dt_tmp$HJD_Kor)]
    # BJD found by HJD_2nd
    survey_dt_tmp$BJD[BJD_N_HJD2_Y_idx] = admincode_dt_tmp$BJD[match(survey_dt_tmp$TX3_2[BJD_N_HJD2_Y_idx], admincode_dt_tmp$HJD_2nd)]
    
    BJD_NA_idx = (is.na(survey_dt_tmp$BJD))
    # survey_dt_tmp$TX3_2[BJD_NA_idx]
    
    survey_dt_tmp$TX3_2_dong = survey_dt_tmp$TX3_2
    survey_dt_tmp$TX3_2_dong[BJD_NA_idx] = paste0(survey_dt_tmp$TX3_2[BJD_NA_idx], "동")
    
    BJD_corrected_Y_idx = survey_dt_tmp$TX3_2_dong %in% admincode_dt_tmp$BJD
    HJD_corrected_Y_idx = survey_dt_tmp$TX3_2_dong %in% admincode_dt_tmp$HJD_Kor
    HJD2_corrected_Y_idx = survey_dt_tmp$TX3_2_dong %in% admincode_dt_tmp$HJD_2nd
    
    BJD_N_HJD_Y_corrected_idx = (!BJD_corrected_Y_idx) & HJD_corrected_Y_idx
    BJD_N_HJD2_Y_corrected_idx = (!BJD_corrected_Y_idx) &  (!HJD_corrected_Y_idx) & HJD2_corrected_Y_idx
    
    
    survey_dt_tmp$BJD[BJD_corrected_Y_idx] = survey_dt_tmp$TX3_2_dong[BJD_corrected_Y_idx]
    survey_dt_tmp$BJD[BJD_N_HJD_Y_corrected_idx] = admincode_dt_tmp$BJD[match(survey_dt_tmp$TX3_2_dong[BJD_N_HJD_Y_corrected_idx], admincode_dt_tmp$HJD_Kor)]
    # BJD found by HJD_2nd
    survey_dt_tmp$BJD[BJD_N_HJD2_Y_corrected_idx] = admincode_dt_tmp$BJD[match(survey_dt_tmp$TX3_2_dong[BJD_N_HJD2_Y_corrected_idx], admincode_dt_tmp$HJD_2nd)]
    
    
    
    
    BJD_NA_idx_step3 = (is.na(survey_dt_tmp$BJD))
    table(BJD_NA_idx_step3)
    table(survey_dt_tmp$TX3_2_dong[BJD_NA_idx_step3])
    table(survey_dt_tmp$TX3_2[BJD_NA_idx_step3])
    
    
    survey_dt_tmp$TX3_2_dong[!BJD_NA_idx_step3] = NA
    survey_dt_tmp$TX3_2_tofix = NA
    survey_dt_tmp$TX3_2_tofix[BJD_NA_idx_step3] = survey_dt_tmp$TX3_2[BJD_NA_idx_step3]
    
    
    
    
    
    
    write.xlsx(survey_dt_tmp, file = paste0("survey_dt_tmp_", regionname_eng[r_idx], "_" , r_code, "_", Sys.Date(), ".xlsx"))
    
}






survey_dt_seoul = survey_dt[survey_dt$Q1==1,] 
length(survey_dt_seoul$TX3_2[survey_dt_seoul$TX3_2 %in% lsmd_seoul$EMD_NM])
(survey_dt_seoul$TX3_2[!(survey_dt_seoul$TX3_2 %in% lsmd_seoul$EMD_NM)])


survey_dt_seoul$TX3_2_RE = survey_dt_seoul$TX3_2


table(survey_dt_seoul$TX3_2 %in% admincode_dt$HJD_Kor)
table(survey_dt_seoul$TX3_2 %in% admincode_dt$BJD)
table(survey_dt_seoul$TX3_2 %in% admincode_dt$BJD)



table(!(survey_dt_seoul$TX3_2 %in% admincode_dt$BJD) & !(survey_dt_seoul$TX3_2 %in% admincode_dt$HJD_Kor))


idx_wrong = (!(survey_dt$TX3_2 %in% admincode_dt$BJD) & !(survey_dt$TX3_2 %in% admincode_dt$HJD_Kor))






# 행정동인 경우



# 동이 없는 경우
survey_dt_seoul$TX3_2_RE[!(survey_dt_seoul$TX3_2 %in% lsmd_seoul$EMD_NM)] = paste0(survey_dt_seoul$TX3_2[!(survey_dt_seoul$TX3_2 %in% lsmd_seoul$EMD_NM)], "동")


(survey_dt_seoul$TX3_2_RE[!(survey_dt_seoul$TX3_2_RE %in% lsmd_seoul$EMD_NM)])

table(survey_dt_seoul$TX3_2 %in% lsmd_seoul$EMD_NM)
table(survey_dt_seoul$TX3_2_RE %in% lsmd_seoul$EMD_NM)

sort(table(survey_dt_seoul$TX3_2_RE[!(survey_dt_seoul$TX3_2_RE %in% lsmd_seoul$EMD_NM)]))



sort(table(survey_dt_seoul$TX3_2_RE[survey_dt_seoul$TX3_2_RE %in% lsmd_seoul$EMD_NM]))



