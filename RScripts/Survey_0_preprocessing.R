library(data.table)
library(doSNOW)
library(openxlsx)
library(readxl)
library(rgdal)
# library(sf)
# devtools::install_github("cardiomoon/Kormaps")
library(Kormaps)
library(dplyr)



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



survey_dt = read_xlsx("생태계서비스팀 공간정보/10. 생태계서비스/2. 생태계서비스 대국민 인식조사 설문결과_19May2023수정.xlsx")

survey_dt = survey_dt[!is.na(survey_dt$No),]

n_cols = 60 
survey_dt = survey_dt[, 1:n_cols]
colnames(survey_dt)[56] = "Gender"
colnames(survey_dt)[57] = "Age"

colnames(survey_dt)
# Q15-Q35: continuous

table(survey_dt$Q1) # Province
table(survey_dt$Q2)# ignore

table(survey_dt$TX3_1)
table(survey_dt$TX3_2) 

table(survey_dt$TX3_3_RE)

# survey_dt$TX3_3_RE = NULL
survey_dt$TX3_3_RE = survey_dt$TX3_3_RE2
survey_dt$TX3_3_RE2 = NULL






### 시군구

# filepath_SGG = "생태계서비스팀 공간정보/01. 각종 경계모음/통계청_행정구역(2022)/시군구행정경계_전국.shp"
# sgg_shp = readOGR(filepath_SGG, use_iconv = TRUE, encoding = "EUC-KR") #

# http://www.gisdeveloper.co.kr/?p=2332
# 법정동 기반 시군구
filepath_SGG2 = "~/Dropbox/GIS Data/Korea/Admin/SIG_202302/sig.shp"
sgg_shp = readOGR(filepath_SGG2, use_iconv = TRUE, encoding = "EUC-KR") #

# 법정동 기반 시군구 단위
sgg_bjd_df = read_xlsx(paste0(path_gis, "Admin/법정동 기준 시군구 단위.xlsx"),4)


stopifnot(all(sgg_shp$SIGUNGU_CD %in% sgg_bjd_df$SGG_CODE))

table(survey_dt$TX3_3_RE %in% sgg_bjd_df$SGGwithRegion)
survey_dt$TX3_3_RE[!(survey_dt$TX3_3_RE %in% sgg_bjd_df$SGGwithRegion)]



### 법정동 
filepath_admincode = "~/Dropbox/GIS Data/Korea/Admin/한국행정구역코드.xlsx"
admincode_dt = read_xlsx(filepath_admincode,sheet = 4, skip = 1) # fourth sheet
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


# Kormaps::areacode
# Kormaps::korpopmap1

survey_province = sapply(survey_dt$TX3_3_RE, FUN = function(x) strsplit(x, " ")[[1]][1])


# matches well?
tb1 = table(survey_dt$Q1, survey_province)
tb1
apply(tb1, MARGIN = 2, FUN = function(x) which(x!=0))


# Fix the raw data
# Seoul 
idx_seoul = survey_province == "서울특별성"
survey_dt[idx_seoul,]$TX3_3_RE = "서울특별시 성동구"


# Incheon 
idx_icn = which((survey_dt$Q1 == 10) & ( survey_province == "인천광역시"))
survey_dt[idx_icn,]$TX3_3_RE
survey_dt[idx_icn,]$Q1 = 2

# Ulsan
idx_ulsan = which((survey_dt$Q1 == 17) & ( survey_province == "울산광역시"))
survey_dt[idx_ulsan,]$TX3_3
survey_dt[idx_ulsan,]$Q1 = 6


# Busan
idx_busan = which((survey_dt$Q1 == 17) & ( survey_province == "부산광역시"))
survey_dt[idx_busan,]$TX3_3
survey_dt[idx_busan,]$Q1 = 7

# now good
tb2 = table(survey_dt$Q1, survey_province)
apply(tb2, MARGIN = 2, FUN = function(x) which(x!=0))


survey_province_corrected = sapply(survey_dt$TX3_3_RE, FUN = function(x) strsplit(x, " ")[[1]][1])

# Sejong-si
survey_dt[survey_province_corrected == "세종특별자치시", ]$Q1


# Q1 
# 1 서울특별시	2 인천광역시	3 대전광역시	4 광주광역시	5 대구광역시	6 울산광역시	7 부산광역시	8 세종특별자치시	9 제주도	10 경기도	11 강원도	12 충청북도	13 충청남도	14 전라북도	15 전라남도	16 경상북도	17 경상남도

# New code 
# 11 서울특별시	28 인천광역시	30 대전광역시	29 광주광역시	27 대구광역시	31 울산광역시	26 부산광역시	36 세종특별자치시	50 제주도	41 경기도	42 강원도	43 충청북도	44 충청남도	45 전라북도	46 전라남도	47 경상북도	48 경상남도


# v1 = survey_province[!(survey_province_corrected %in% Kormaps::areacode$name)]
# as.vector(v1) # 세종시 별도 처리


regioncode_old = c(1:17)
regioncode_new = c(11, 28, 30, 29, 27, 31, 26, 36, 50, 41, 42, 43, 44, 45, 46, 47, 48)
regionname_kor = c("서울특별시", "인천광역시", "대전광역시", "광주광역시", "대구광역시", "울산광역시", "부산광역시", "세종특별자치시", "제주도", "경기도", "강원도", "충청북도", "충청남도", "전라북도", "전라남도", "경상북도", "경상남도")
regionname_eng = c("Seoul", "Incheon", "Daejeon", "Gwangju", "Daegu", "Ulsan", "Busan", "Sejon", "Jeju", "Gyeonggi", "Gangwon", "Chungbuk", "Chungnam", "Jeonbuk", "Jeonnam", "Gyeongbuk", "Gyeongnam")


survey_dt$Q1_new = regioncode_new[survey_dt$Q1]


admincode_dt$Regioncode = regioncode_new[match(admincode_dt$Region,regionname_kor)]


# remove whitespaces
survey_dt$TX3_2 = stringr::str_trim(survey_dt$TX3_2)
survey_dt$TX3_2  = stringr::str_remove_all(survey_dt$TX3_2, pattern = " ")



TX3_3_spl = foreach (tx3_3 = survey_dt$TX3_3_RE, .combine = "rbind") %do% { 
    
    spl = strsplit(tx3_3, " ")[[1]]
    if (length(spl) == 1) {
        spl = c(spl, "")   
    } else if (length(spl) == 2) {
        # do nothing
        
    } else if (length(spl) == 3) {
        spl = c(spl[1], paste(spl[2], spl[3])   )
        
    } else {
        stop("shouln't be more than three elements")
        
    }
    
    return(spl)
}

colnames(TX3_3_spl) = c("Region", "SGG")

survey_dt_new = cbind(survey_dt, TX3_3_spl)




# 원자료 오타 수정
survey_dt_new$SGG [survey_dt_new$SGG == "동장구" ] = "동작구"
survey_dt_new$SGG [survey_dt_new$SGG == "진해시" ] = "창원시 진해구" # 창원시로 편입
survey_dt_new$SGG [survey_dt_new$SGG == "성주시" ] = "성주군"
survey_dt_new$SGG [survey_dt_new$SGG == "연기면" ] = "세종특별자치시" # 세종시로 편입
survey_dt_new$SGG [survey_dt_new$SGG == "상서구" ] = "강서구"

# table(survey_dt_new$SGG %in% admincode_dt$SGG)
# table(survey_dt_new$SGG[!(survey_dt_new$SGG %in% admincode_dt$SGG)])


# matching SGG code 

# sggcode_df_small = data.frame(sggcode_dt[, c( "SGG_Code", "Region", "SGG")])
# sggcode_df_small = sggcode_df_small[!duplicated(sggcode_df_small),]



# first match region and then SGG
# survey_dt_new_merged = merge(data.frame(survey_dt_new[,]),y=sggcode_df_small,  by.x = c("Region", "SGG"), by.y = c("Region", "SGG"),  all.x=T, all.y = F)


survey_dt_new_merged = merge(data.frame(survey_dt_new[,]),y=sgg_bjd_df[,c("SGGwithRegion", "SGG_CODE")],  by.x = c("TX3_3_RE"), by.y = c("SGGwithRegion"), all.x=T, all.y = F)


nrow(survey_dt_new_merged)


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









for (r_idx in seq_along(regioncode_new)) { 
    
    r_code= regioncode_new[r_idx]
    
    survey_dt_tmp = survey_dt[survey_dt$Q1_new == r_code,]
    admincode_dt_tmp = admincode_dt[admincode_dt$Regioncode == r_code,]
    
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


library(dplyr)

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



