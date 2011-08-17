MODULE mod_iratlas
  ! Description:
  !   Data and routines for IR emissivity atlas.
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0      02/06/2010  Based on UW IR atlas code (E. Borbas, B. Ruston, J. Hocking)
  !
  USE Type_Kinds, ONLY: fp 
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: crtm_uwiremis
  PUBLIC :: crtm_uwiremis_init 

  PUBLIC :: surftype_land 
  PUBLIC :: surftype_sea
  PUBLIC :: surftype_seaice
  PUBLIC :: ir_atlas_version 
  PUBLIC :: crtm_uwiremis_close_atlas
  Integer, Parameter :: surftype_land = 0
  Integer, Parameter :: surftype_sea = 1
  Integer, Parameter :: surftype_seaice = 2

  ! Specify kinds for NetCDF interface
  INTEGER, PARAMETER :: ncint32  = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: ncint16  = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: ncint8   = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER :: ncreal32 = SELECTED_REAL_KIND(6,37)
  
  ! User can specify atlas version at set-up. Version 100 is the default.
  INTEGER :: ir_atlas_version=100   ! Version of atlas
  
  ! Atlas constants
    
  INTEGER, PARAMETER :: numpcs=6
  INTEGER, PARAMETER :: hngpnts=10
  INTEGER, PARAMETER :: numwave=416

  INTEGER, PARAMETER :: nb_lats=1800
  INTEGER, PARAMETER :: nb_lons=3600
  ! INTEGER, PARAMETER :: nb_pack=2298394  !ver2.1  2006
  INTEGER, PARAMETER :: nb_pack=2250931  !ver2.1  2007

  INTEGER, PARAMETER :: cv_lats=360
  INTEGER, PARAMETER :: cv_lons=720
  INTEGER, PARAMETER :: cv_pack=98008

  INTEGER, PARAMETER :: db_ver_year=2007
  
  INTEGER, PARAMETER :: seaice_flag=70          ! flag value returned for sea-ice
  REAL(fp),    PARAMETER :: default_std = 0.05_fp ! default standard deviation

  REAL(fp),    PARAMETER :: bfemis_gridres = 0.1_fp
  REAL(fp),    PARAMETER :: bfemis_ygrid1 = 89.95_fp
  REAL(fp),    PARAMETER :: bfemis_xgrid1 = -179.95_fp

  REAL(fp),    PARAMETER :: cov_emis_gridres = 0.5_fp
  REAL(fp),    PARAMETER :: cov_emis_ygrid1 = 89.75_fp
  REAL(fp),    PARAMETER :: cov_emis_xgrid1 = -179.75_fp
  
  
  ! Atlas data loaded by initialisation routine
  
  INTEGER(KIND=ncint16), ALLOCATABLE :: bfemis_flag(:,:)  ! dims are (nb_lats,nb_lons)
  INTEGER,    ALLOCATABLE :: bfemis_lut(:,:)   ! dims are (nb_lats,nb_lons)
  REAL(fp),       ALLOCATABLE :: bfemis(:,:)       ! dims are (nb_pack,hngpnts)

  INTEGER,    ALLOCATABLE :: cov_emis_lut(:,:) ! dims are (cv_lats,cv_lons)
  REAL(fp),       ALLOCATABLE :: cov_emis(:,:)     ! dims are (cv_pack,numwave)

  REAL(fp)     :: D(hngpnts,numpcs)
  REAL(KIND=ncreal32) :: pcu(numwave,numwave)              ! pcu need to be read in from flat file  
  
  
  ! Atlas data contained in this file
  
  REAL(fp) :: sice_em(numwave), snow_em(numwave)
  REAL(fp) :: sice_stdv, snow_stdv
  REAL(fp) :: pcm(numwave)
  REAL(fp) :: hsr_wavenum(numwave)
  REAL(fp) :: pcu_modres(hngpnts,hngpnts)
  REAL(fp) :: pcm_modres(hngpnts)


  DATA pcm_modres / &
        0.9782182_fp,   0.9759824_fp,   0.9681804_fp,   0.9187206_fp,   0.9265846_fp,  &
        0.9733831_fp,   0.9684200_fp,   0.9459322_fp,   0.8725729_fp,   0.8704424_fp/

!   number of PC is the first dimension and spectra is the second for pcu_modres (eigenvectors) 
  DATA pcu_modres / &
       -0.0036189_fp,  -0.0107326_fp,  -0.0357575_fp,  -0.0199025_fp,  -0.0268988_fp,  &
       -0.0458035_fp,  -0.1067330_fp,  -0.1013470_fp,  -0.0089873_fp,   0.0614024_fp,  &
       -0.0024519_fp,  -0.0111489_fp,  -0.0552112_fp,   0.0015246_fp,  -0.0248343_fp,  &
       -0.0227242_fp,  -0.0621471_fp,  -0.0977514_fp,  -0.0291845_fp,   0.0449263_fp,  &
        0.0030866_fp,   0.0003667_fp,  -0.0769359_fp,   0.0242423_fp,   0.0167816_fp,  &
       -0.0266578_fp,  -0.0145453_fp,  -0.0207119_fp,  -0.0309243_fp,  -0.0287100_fp,  &
        0.0362763_fp,   0.1269156_fp,  -0.0941633_fp,  -0.0524558_fp,   0.0621878_fp,  &
       -0.0331482_fp,   0.0164898_fp,  -0.0062478_fp,   0.0276787_fp,   0.0014637_fp,  &
        0.0393517_fp,   0.1177192_fp,  -0.0421300_fp,  -0.1202559_fp,  -0.0664961_fp,  &
        0.0741313_fp,   0.0287669_fp,   0.0501746_fp,   0.0506316_fp,   0.0914001_fp,  &
        0.0002204_fp,  -0.0087577_fp,  -0.0449563_fp,   0.0327884_fp,  -0.0167350_fp,  &
        0.0526362_fp,  -0.0313638_fp,   0.0499965_fp,   0.0695323_fp,   0.0616089_fp,  &
        0.0112971_fp,  -0.0362747_fp,  -0.0328459_fp,   0.0031088_fp,  -0.0465960_fp,  &
       -0.0110655_fp,  -0.0633873_fp,   0.0473527_fp,   0.0344864_fp,  -0.0452882_fp,  &
        0.0207165_fp,  -0.0652447_fp,  -0.0579818_fp,   0.0071345_fp,  -0.0130217_fp,  &
       -0.0274127_fp,   0.0848268_fp,   0.0065488_fp,   0.0052432_fp,  -0.0071355_fp,  &
        0.0836001_fp,  -0.0290997_fp,  -0.0019292_fp,  -0.0057489_fp,   0.0253799_fp,  &
        0.0964016_fp,   0.0197362_fp,   0.0286795_fp,  -0.1536087_fp,   0.0228268_fp,  &
        0.0887494_fp,   0.0377112_fp,   0.0174746_fp,   0.1030488_fp,  -0.0500023_fp,  &
        0.0222185_fp,   0.0446731_fp,  -0.0709027_fp,   0.0504857_fp,   0.0019073_fp/

  DATA hsr_wavenum / &
    699.3_fp,  704.3_fp,  709.3_fp,  714.3_fp,  719.3_fp,  724.3_fp,  729.3_fp,  734.3_fp,  &
    739.3_fp,  744.3_fp,  749.3_fp,  754.3_fp,  759.3_fp,  764.3_fp,  769.3_fp,  774.3_fp,  &
    779.3_fp,  784.3_fp,  789.3_fp,  794.3_fp,  799.3_fp,  804.3_fp,  809.3_fp,  814.3_fp,  &
    819.3_fp,  824.3_fp,  829.3_fp,  834.3_fp,  839.3_fp,  844.3_fp,  849.3_fp,  854.3_fp,  &
    859.3_fp,  864.3_fp,  869.3_fp,  874.3_fp,  879.3_fp,  884.3_fp,  889.3_fp,  894.3_fp,  &
    899.3_fp,  904.3_fp,  909.3_fp,  914.3_fp,  919.3_fp,  924.3_fp,  929.3_fp,  934.3_fp,  &
    939.3_fp,  944.3_fp,  949.3_fp,  954.3_fp,  959.3_fp,  964.3_fp,  969.3_fp,  974.3_fp,  &
    979.3_fp,  984.3_fp,  989.3_fp,  994.3_fp,  999.3_fp, 1004.3_fp, 1009.3_fp, 1014.3_fp,  &
    1019.3_fp, 1024.3_fp, 1029.3_fp, 1034.3_fp, 1039.3_fp, 1044.3_fp, 1049.3_fp, 1054.3_fp,  &
    1059.3_fp, 1064.3_fp, 1069.3_fp, 1074.3_fp, 1079.3_fp, 1084.3_fp, 1089.3_fp, 1094.3_fp,  &
    1099.3_fp, 1104.3_fp, 1109.3_fp, 1114.3_fp, 1119.3_fp, 1124.3_fp, 1129.3_fp, 1134.3_fp,  &
    1139.3_fp, 1144.3_fp, 1149.3_fp, 1154.3_fp, 1159.3_fp, 1164.3_fp, 1169.3_fp, 1174.3_fp,  &
    1179.3_fp, 1184.3_fp, 1189.3_fp, 1194.3_fp, 1199.3_fp, 1204.3_fp, 1209.3_fp, 1214.3_fp,  &
    1219.3_fp, 1224.3_fp, 1229.3_fp, 1234.3_fp, 1239.3_fp, 1244.3_fp, 1249.3_fp, 1254.3_fp,  &
    1259.3_fp, 1264.3_fp, 1269.3_fp, 1274.3_fp, 1279.3_fp, 1284.3_fp, 1289.3_fp, 1294.3_fp,  &
    1299.3_fp, 1304.3_fp, 1309.3_fp, 1314.3_fp, 1319.3_fp, 1324.3_fp, 1329.3_fp, 1334.3_fp,  &
    1339.3_fp, 1344.3_fp, 1349.3_fp, 1354.3_fp, 1359.3_fp, 1364.3_fp, 1369.3_fp, 1374.3_fp,  &
    1379.3_fp, 1384.3_fp, 1389.3_fp, 1394.3_fp, 1399.3_fp, 1404.3_fp, 1409.3_fp, 1414.3_fp,  &
    1419.3_fp, 1424.3_fp, 1429.3_fp, 1434.3_fp, 1439.3_fp, 1444.3_fp, 1449.3_fp, 1454.3_fp,  &
    1459.3_fp, 1464.3_fp, 1469.3_fp, 1474.3_fp, 1479.3_fp, 1484.3_fp, 1489.3_fp, 1494.3_fp,  &
    1499.3_fp, 1504.3_fp, 1509.3_fp, 1514.3_fp, 1519.3_fp, 1524.3_fp, 1529.3_fp, 1534.3_fp,  &
    1539.3_fp, 1544.3_fp, 1549.3_fp, 1554.3_fp, 1559.3_fp, 1564.3_fp, 1569.3_fp, 1574.3_fp,  &
    1579.3_fp, 1584.3_fp, 1589.3_fp, 1594.3_fp, 1599.3_fp, 1604.3_fp, 1609.3_fp, 1614.3_fp,  &
    1619.3_fp, 1624.3_fp, 1629.3_fp, 1634.3_fp, 1639.3_fp, 1644.3_fp, 1649.3_fp, 1654.3_fp,  &
    1659.3_fp, 1664.3_fp, 1669.3_fp, 1674.3_fp, 1679.3_fp, 1684.3_fp, 1689.3_fp, 1694.3_fp,  &
    1699.3_fp, 1704.3_fp, 1709.3_fp, 1714.3_fp, 1719.3_fp, 1724.3_fp, 1729.3_fp, 1734.3_fp,  &
    1739.3_fp, 1744.3_fp, 1749.3_fp, 1754.3_fp, 1759.3_fp, 1764.3_fp, 1769.3_fp, 1774.3_fp,  &
    1779.3_fp, 1784.3_fp, 1789.3_fp, 1794.3_fp, 1799.3_fp, 1804.3_fp, 1809.3_fp, 1814.3_fp,  &
    1819.3_fp, 1824.3_fp, 1829.3_fp, 1834.3_fp, 1839.3_fp, 1844.3_fp, 1849.3_fp, 1854.3_fp,  &
    1859.3_fp, 1864.3_fp, 1869.3_fp, 1874.3_fp, 1879.3_fp, 1884.3_fp, 1889.3_fp, 1894.3_fp,  &
    1899.3_fp, 1904.3_fp, 1909.3_fp, 1914.3_fp, 1919.3_fp, 1924.3_fp, 1929.3_fp, 1934.3_fp,  &
    1939.3_fp, 1944.3_fp, 1949.3_fp, 1954.3_fp, 1959.3_fp, 1964.3_fp, 1969.3_fp, 1974.3_fp,  &
    1979.3_fp, 1984.3_fp, 1989.3_fp, 1994.3_fp, 1999.3_fp, 2004.3_fp, 2009.3_fp, 2014.3_fp,  &
    2019.3_fp, 2024.3_fp, 2029.3_fp, 2034.3_fp, 2039.3_fp, 2044.3_fp, 2049.3_fp, 2054.3_fp,  &
    2059.3_fp, 2064.3_fp, 2069.3_fp, 2074.3_fp, 2079.3_fp, 2084.3_fp, 2089.3_fp, 2094.3_fp,  &
    2099.3_fp, 2104.3_fp, 2109.3_fp, 2114.3_fp, 2119.3_fp, 2124.3_fp, 2129.3_fp, 2134.3_fp,  &
    2139.3_fp, 2144.3_fp, 2149.3_fp, 2154.3_fp, 2159.3_fp, 2164.3_fp, 2169.3_fp, 2174.3_fp,  &
    2179.3_fp, 2184.3_fp, 2189.3_fp, 2194.3_fp, 2199.3_fp, 2204.3_fp, 2209.3_fp, 2214.3_fp,  &
    2219.3_fp, 2224.3_fp, 2229.3_fp, 2234.3_fp, 2239.3_fp, 2244.3_fp, 2249.3_fp, 2254.3_fp,  &
    2259.3_fp, 2264.3_fp, 2269.3_fp, 2274.3_fp, 2279.3_fp, 2284.3_fp, 2289.3_fp, 2294.3_fp,  &
    2299.3_fp, 2304.3_fp, 2309.3_fp, 2314.3_fp, 2319.3_fp, 2324.3_fp, 2329.3_fp, 2334.3_fp,  &
    2339.3_fp, 2344.3_fp, 2349.3_fp, 2354.3_fp, 2359.3_fp, 2364.3_fp, 2369.3_fp, 2374.3_fp,  &
    2379.3_fp, 2384.3_fp, 2389.3_fp, 2394.3_fp, 2399.3_fp, 2404.3_fp, 2409.3_fp, 2414.3_fp,  &
    2419.3_fp, 2424.3_fp, 2429.3_fp, 2434.3_fp, 2439.3_fp, 2444.3_fp, 2449.3_fp, 2454.3_fp,  &
    2459.3_fp, 2464.3_fp, 2469.3_fp, 2474.3_fp, 2479.3_fp, 2484.3_fp, 2489.3_fp, 2494.3_fp,  &
    2499.3_fp, 2504.3_fp, 2509.3_fp, 2514.3_fp, 2519.3_fp, 2524.3_fp, 2529.3_fp, 2534.3_fp,  &
    2539.3_fp, 2544.3_fp, 2549.3_fp, 2554.3_fp, 2559.3_fp, 2564.3_fp, 2569.3_fp, 2574.3_fp,  &
    2579.3_fp, 2584.3_fp, 2589.3_fp, 2594.3_fp, 2599.3_fp, 2604.3_fp, 2609.3_fp, 2614.3_fp,  &
    2619.3_fp, 2624.3_fp, 2629.3_fp, 2634.3_fp, 2639.3_fp, 2644.3_fp, 2649.3_fp, 2654.3_fp,  &
    2659.3_fp, 2664.3_fp, 2669.3_fp, 2674.3_fp, 2679.3_fp, 2684.3_fp, 2689.3_fp, 2694.3_fp,  &
    2699.3_fp, 2704.3_fp, 2709.3_fp, 2714.3_fp, 2719.3_fp, 2724.3_fp, 2729.3_fp, 2734.3_fp,  &
    2739.3_fp, 2744.3_fp, 2749.3_fp, 2754.3_fp, 2759.3_fp, 2764.3_fp, 2769.3_fp, 2774.3_fp /

  DATA pcm / &
        0.9782182_fp,   0.9770744_fp,   0.9763290_fp,   0.9763215_fp,   0.9760258_fp,  &
        0.9763704_fp,   0.9767076_fp,   0.9763077_fp,   0.9758835_fp,   0.9753462_fp,  &
        0.9748067_fp,   0.9734465_fp,   0.9721510_fp,   0.9717180_fp,   0.9714773_fp,  &
        0.9706340_fp,   0.9710826_fp,   0.9722888_fp,   0.9731166_fp,   0.9732918_fp,  &
        0.9736975_fp,   0.9751787_fp,   0.9770049_fp,   0.9773170_fp,   0.9765164_fp,  &
        0.9759824_fp,   0.9750199_fp,   0.9746831_fp,   0.9738413_fp,   0.9731615_fp,  &
        0.9720387_fp,   0.9716908_fp,   0.9708628_fp,   0.9705366_fp,   0.9697853_fp,  &
        0.9694459_fp,   0.9688896_fp,   0.9688236_fp,   0.9689180_fp,   0.9692774_fp,  &
        0.9693237_fp,   0.9692513_fp,   0.9689918_fp,   0.9686664_fp,   0.9684489_fp,  &
        0.9681804_fp,   0.9672847_fp,   0.9667084_fp,   0.9661347_fp,   0.9655386_fp,  &
        0.9650131_fp,   0.9641176_fp,   0.9628995_fp,   0.9620982_fp,   0.9605948_fp,  &
        0.9590283_fp,   0.9572537_fp,   0.9552648_fp,   0.9529146_fp,   0.9505763_fp,  &
        0.9486620_fp,   0.9468448_fp,   0.9446425_fp,   0.9428397_fp,   0.9415421_fp,  &
        0.9398234_fp,   0.9378662_fp,   0.9358756_fp,   0.9338515_fp,   0.9317511_fp,  &
        0.9296144_fp,   0.9274116_fp,   0.9248639_fp,   0.9219664_fp,   0.9197029_fp,  &
        0.9187206_fp,   0.9195539_fp,   0.9211251_fp,   0.9227578_fp,   0.9242273_fp,  &
        0.9256495_fp,   0.9265392_fp,   0.9276078_fp,   0.9279289_fp,   0.9282181_fp,  &
        0.9284544_fp,   0.9289097_fp,   0.9299400_fp,   0.9314128_fp,   0.9329405_fp,  &
        0.9349486_fp,   0.9377099_fp,   0.9380918_fp,   0.9354525_fp,   0.9330018_fp,  &
        0.9316696_fp,   0.9308965_fp,   0.9296793_fp,   0.9282659_fp,   0.9273711_fp,  &
        0.9268156_fp,   0.9265846_fp,   0.9264724_fp,   0.9278417_fp,   0.9298262_fp,  &
        0.9342009_fp,   0.9397170_fp,   0.9451398_fp,   0.9501663_fp,   0.9547508_fp,  &
        0.9586911_fp,   0.9618842_fp,   0.9649577_fp,   0.9675525_fp,   0.9696881_fp,  &
        0.9708689_fp,   0.9717879_fp,   0.9722518_fp,   0.9724457_fp,   0.9728941_fp,  &
        0.9731293_fp,   0.9731925_fp,   0.9730867_fp,   0.9733831_fp,   0.9735166_fp,  &
        0.9740434_fp,   0.9742066_fp,   0.9746855_fp,   0.9748268_fp,   0.9749292_fp,  &
        0.9751188_fp,   0.9752902_fp,   0.9751062_fp,   0.9751985_fp,   0.9752622_fp,  &
        0.9750626_fp,   0.9755121_fp,   0.9755228_fp,   0.9760818_fp,   0.9759580_fp,  &
        0.9758280_fp,   0.9755163_fp,   0.9754220_fp,   0.9750829_fp,   0.9743836_fp,  &
        0.9745844_fp,   0.9742978_fp,   0.9740397_fp,   0.9744191_fp,   0.9745796_fp,  &
        0.9749123_fp,   0.9750853_fp,   0.9746974_fp,   0.9747824_fp,   0.9746920_fp,  &
        0.9735873_fp,   0.9733123_fp,   0.9725510_fp,   0.9718717_fp,   0.9713586_fp,  &
        0.9706160_fp,   0.9701124_fp,   0.9698699_fp,   0.9698430_fp,   0.9694992_fp,  &
        0.9691019_fp,   0.9690002_fp,   0.9678345_fp,   0.9668854_fp,   0.9659764_fp,  &
        0.9666998_fp,   0.9669611_fp,   0.9665817_fp,   0.9679645_fp,   0.9695909_fp,  &
        0.9711555_fp,   0.9724632_fp,   0.9737635_fp,   0.9746142_fp,   0.9748497_fp,  &
        0.9752109_fp,   0.9752749_fp,   0.9754022_fp,   0.9753313_fp,   0.9746057_fp,  &
        0.9745884_fp,   0.9747860_fp,   0.9752877_fp,   0.9753085_fp,   0.9759305_fp,  &
        0.9752344_fp,   0.9748027_fp,   0.9757417_fp,   0.9751943_fp,   0.9748128_fp,  &
        0.9743713_fp,   0.9741939_fp,   0.9725359_fp,   0.9723988_fp,   0.9716700_fp,  &
        0.9708291_fp,   0.9705051_fp,   0.9699901_fp,   0.9689955_fp,   0.9683419_fp,  &
        0.9684200_fp,   0.9672046_fp,   0.9660766_fp,   0.9658424_fp,   0.9648336_fp,  &
        0.9640325_fp,   0.9642861_fp,   0.9636880_fp,   0.9638920_fp,   0.9638573_fp,  &
        0.9641714_fp,   0.9648057_fp,   0.9648220_fp,   0.9639065_fp,   0.9635883_fp,  &
        0.9626419_fp,   0.9616417_fp,   0.9600965_fp,   0.9587714_fp,   0.9576451_fp,  &
        0.9557189_fp,   0.9545730_fp,   0.9550443_fp,   0.9551759_fp,   0.9560625_fp,  &
        0.9576327_fp,   0.9587138_fp,   0.9594474_fp,   0.9598546_fp,   0.9601094_fp,  &
        0.9601356_fp,   0.9597549_fp,   0.9590299_fp,   0.9581512_fp,   0.9572046_fp,  &
        0.9557602_fp,   0.9538486_fp,   0.9521495_fp,   0.9503905_fp,   0.9491790_fp,  &
        0.9485527_fp,   0.9479896_fp,   0.9475234_fp,   0.9468080_fp,   0.9469628_fp,  &
        0.9469683_fp,   0.9465806_fp,   0.9468755_fp,   0.9466828_fp,   0.9471480_fp,  &
        0.9470276_fp,   0.9470209_fp,   0.9468378_fp,   0.9464890_fp,   0.9462101_fp,  &
        0.9459322_fp,   0.9449111_fp,   0.9435923_fp,   0.9416961_fp,   0.9401403_fp,  &
        0.9387150_fp,   0.9374595_fp,   0.9347988_fp,   0.9319339_fp,   0.9295776_fp,  &
        0.9268476_fp,   0.9243815_fp,   0.9224647_fp,   0.9208075_fp,   0.9195780_fp,  &
        0.9183103_fp,   0.9171674_fp,   0.9164810_fp,   0.9160877_fp,   0.9151877_fp,  &
        0.9148492_fp,   0.9142842_fp,   0.9142084_fp,   0.9138089_fp,   0.9137760_fp,  &
        0.9137531_fp,   0.9141592_fp,   0.9136598_fp,   0.9125727_fp,   0.9108481_fp,  &
        0.9093652_fp,   0.9080561_fp,   0.9062355_fp,   0.9046820_fp,   0.9028210_fp,  &
        0.9018152_fp,   0.9008504_fp,   0.9000632_fp,   0.8995758_fp,   0.8989593_fp,  &
        0.8987811_fp,   0.8992507_fp,   0.8999549_fp,   0.9013391_fp,   0.9020863_fp,  &
        0.9025120_fp,   0.9023982_fp,   0.9015658_fp,   0.9008633_fp,   0.8996401_fp,  &
        0.8981582_fp,   0.8969440_fp,   0.8946483_fp,   0.8925536_fp,   0.8906261_fp,  &
        0.8889833_fp,   0.8870751_fp,   0.8845615_fp,   0.8825631_fp,   0.8811586_fp,  &
        0.8796447_fp,   0.8779839_fp,   0.8765292_fp,   0.8754975_fp,   0.8739760_fp,  &
        0.8725729_fp,   0.8714029_fp,   0.8706908_fp,   0.8710466_fp,   0.8699325_fp,  &
        0.8697992_fp,   0.8718969_fp,   0.8713725_fp,   0.8701416_fp,   0.8695096_fp,  &
        0.8698574_fp,   0.8700698_fp,   0.8694080_fp,   0.8693934_fp,   0.8693246_fp,  &
        0.8698239_fp,   0.8696592_fp,   0.8681608_fp,   0.8656288_fp,   0.8654716_fp,  &
        0.8640761_fp,   0.8639477_fp,   0.8635154_fp,   0.8630069_fp,   0.8623275_fp,  &
        0.8623751_fp,   0.8627441_fp,   0.8630516_fp,   0.8638958_fp,   0.8644919_fp,  &
        0.8655882_fp,   0.8666160_fp,   0.8676174_fp,   0.8692035_fp,   0.8695340_fp,  &
        0.8703975_fp,   0.8714244_fp,   0.8715467_fp,   0.8713564_fp,   0.8712272_fp,  &
        0.8714187_fp,   0.8701625_fp,   0.8697796_fp,   0.8688766_fp,   0.8682391_fp,  &
        0.8680181_fp,   0.8676605_fp,   0.8672657_fp,   0.8679592_fp,   0.8675538_fp,  &
        0.8686572_fp,   0.8682060_fp,   0.8688578_fp,   0.8693632_fp,   0.8689557_fp,  &
        0.8681611_fp,   0.8684876_fp,   0.8680010_fp,   0.8675498_fp,   0.8675414_fp,  &
        0.8677824_fp,   0.8665875_fp,   0.8668503_fp,   0.8665696_fp,   0.8671130_fp,  &
        0.8669835_fp,   0.8671956_fp,   0.8683699_fp,   0.8685648_fp,   0.8682314_fp,  &
        0.8683055_fp,   0.8694246_fp,   0.8689486_fp,   0.8693868_fp,   0.8694460_fp,  &
        0.8701811_fp,   0.8704424_fp,   0.8709887_fp,   0.8712862_fp,   0.8721344_fp,  &
        0.8724745_fp,   0.8727338_fp,   0.8740577_fp,   0.8748575_fp,   0.8747587_fp,  &
        0.8762293_fp,   0.8772818_fp,   0.8779803_fp,   0.8791369_fp,   0.8807610_fp,  &
        0.8813813_fp/

  DATA sice_stdv / 0.015_fp /
  DATA sice_em / &
      0.9370_fp, 0.9370_fp, 0.9370_fp, 0.9370_fp, 0.9367_fp, 0.9367_fp, 0.9366_fp, 0.9365_fp, &
      0.9365_fp, 0.9365_fp, 0.9365_fp, 0.9366_fp, 0.9367_fp, 0.9370_fp, 0.9374_fp, 0.9381_fp, &
      0.9386_fp, 0.9393_fp, 0.9401_fp, 0.9408_fp, 0.9415_fp, 0.9427_fp, 0.9440_fp, 0.9452_fp, &
      0.9464_fp, 0.9481_fp, 0.9496_fp, 0.9511_fp, 0.9525_fp, 0.9544_fp, 0.9563_fp, 0.9582_fp, &
      0.9602_fp, 0.9620_fp, 0.9640_fp, 0.9658_fp, 0.9678_fp, 0.9702_fp, 0.9725_fp, 0.9748_fp, &
      0.9770_fp, 0.9792_fp, 0.9814_fp, 0.9836_fp, 0.9856_fp, 0.9872_fp, 0.9885_fp, 0.9897_fp, &
      0.9905_fp, 0.9911_fp, 0.9913_fp, 0.9913_fp, 0.9912_fp, 0.9910_fp, 0.9907_fp, 0.9904_fp, &
      0.9901_fp, 0.9897_fp, 0.9893_fp, 0.9889_fp, 0.9885_fp, 0.9880_fp, 0.9876_fp, 0.9871_fp, &
      0.9867_fp, 0.9864_fp, 0.9861_fp, 0.9858_fp, 0.9854_fp, 0.9852_fp, 0.9849_fp, 0.9846_fp, &
      0.9844_fp, 0.9842_fp, 0.9840_fp, 0.9838_fp, 0.9836_fp, 0.9834_fp, 0.9832_fp, 0.9831_fp, &
      0.9829_fp, 0.9828_fp, 0.9826_fp, 0.9824_fp, 0.9822_fp, 0.9821_fp, 0.9820_fp, 0.9819_fp, &
      0.9817_fp, 0.9816_fp, 0.9814_fp, 0.9813_fp, 0.9811_fp, 0.9810_fp, 0.9808_fp, 0.9807_fp, &
      0.9805_fp, 0.9804_fp, 0.9803_fp, 0.9801_fp, 0.9799_fp, 0.9797_fp, 0.9796_fp, 0.9794_fp, &
      0.9792_fp, 0.9791_fp, 0.9789_fp, 0.9787_fp, 0.9786_fp, 0.9785_fp, 0.9784_fp, 0.9784_fp, &
      0.9783_fp, 0.9782_fp, 0.9782_fp, 0.9782_fp, 0.9781_fp, 0.9781_fp, 0.9781_fp, 0.9781_fp, &
      0.9781_fp, 0.9781_fp, 0.9781_fp, 0.9780_fp, 0.9780_fp, 0.9780_fp, 0.9780_fp, 0.9780_fp, &
      0.9780_fp, 0.9780_fp, 0.9779_fp, 0.9779_fp, 0.9778_fp, 0.9778_fp, 0.9777_fp, 0.9777_fp, &
      0.9777_fp, 0.9776_fp, 0.9776_fp, 0.9776_fp, 0.9776_fp, 0.9776_fp, 0.9776_fp, 0.9776_fp, &
      0.9776_fp, 0.9776_fp, 0.9776_fp, 0.9776_fp, 0.9776_fp, 0.9775_fp, 0.9775_fp, 0.9775_fp, &
      0.9776_fp, 0.9776_fp, 0.9776_fp, 0.9776_fp, 0.9777_fp, 0.9777_fp, 0.9777_fp, 0.9777_fp, &
      0.9777_fp, 0.9777_fp, 0.9777_fp, 0.9777_fp, 0.9777_fp, 0.9776_fp, 0.9776_fp, 0.9776_fp, &
      0.9775_fp, 0.9775_fp, 0.9774_fp, 0.9773_fp, 0.9773_fp, 0.9773_fp, 0.9773_fp, 0.9773_fp, &
      0.9774_fp, 0.9774_fp, 0.9775_fp, 0.9776_fp, 0.9777_fp, 0.9778_fp, 0.9779_fp, 0.9780_fp, &
      0.9781_fp, 0.9782_fp, 0.9783_fp, 0.9785_fp, 0.9786_fp, 0.9788_fp, 0.9790_fp, 0.9792_fp, &
      0.9793_fp, 0.9795_fp, 0.9797_fp, 0.9799_fp, 0.9801_fp, 0.9802_fp, 0.9803_fp, 0.9805_fp, &
      0.9806_fp, 0.9807_fp, 0.9808_fp, 0.9809_fp, 0.9810_fp, 0.9811_fp, 0.9811_fp, 0.9811_fp, &
      0.9810_fp, 0.9810_fp, 0.9810_fp, 0.9809_fp, 0.9808_fp, 0.9808_fp, 0.9807_fp, 0.9807_fp, &
      0.9806_fp, 0.9805_fp, 0.9805_fp, 0.9804_fp, 0.9803_fp, 0.9802_fp, 0.9802_fp, 0.9801_fp, &
      0.9800_fp, 0.9799_fp, 0.9798_fp, 0.9797_fp, 0.9797_fp, 0.9795_fp, 0.9795_fp, 0.9794_fp, &
      0.9793_fp, 0.9792_fp, 0.9791_fp, 0.9791_fp, 0.9789_fp, 0.9789_fp, 0.9788_fp, 0.9787_fp, &
      0.9786_fp, 0.9785_fp, 0.9785_fp, 0.9783_fp, 0.9783_fp, 0.9782_fp, 0.9781_fp, 0.9781_fp, &
      0.9780_fp, 0.9779_fp, 0.9779_fp, 0.9778_fp, 0.9777_fp, 0.9777_fp, 0.9776_fp, 0.9775_fp, &
      0.9774_fp, 0.9774_fp, 0.9773_fp, 0.9772_fp, 0.9771_fp, 0.9771_fp, 0.9770_fp, 0.9769_fp, &
      0.9769_fp, 0.9768_fp, 0.9767_fp, 0.9766_fp, 0.9765_fp, 0.9765_fp, 0.9764_fp, 0.9764_fp, &
      0.9763_fp, 0.9762_fp, 0.9762_fp, 0.9761_fp, 0.9761_fp, 0.9760_fp, 0.9759_fp, 0.9758_fp, &
      0.9757_fp, 0.9757_fp, 0.9756_fp, 0.9756_fp, 0.9755_fp, 0.9755_fp, 0.9754_fp, 0.9754_fp, &
      0.9754_fp, 0.9754_fp, 0.9754_fp, 0.9753_fp, 0.9753_fp, 0.9753_fp, 0.9753_fp, 0.9752_fp, &
      0.9752_fp, 0.9752_fp, 0.9753_fp, 0.9754_fp, 0.9755_fp, 0.9756_fp, 0.9757_fp, 0.9757_fp, &
      0.9758_fp, 0.9758_fp, 0.9759_fp, 0.9759_fp, 0.9760_fp, 0.9760_fp, 0.9760_fp, 0.9760_fp, &
      0.9761_fp, 0.9761_fp, 0.9761_fp, 0.9761_fp, 0.9761_fp, 0.9761_fp, 0.9761_fp, 0.9761_fp, &
      0.9761_fp, 0.9760_fp, 0.9760_fp, 0.9759_fp, 0.9759_fp, 0.9758_fp, 0.9758_fp, 0.9757_fp, &
      0.9757_fp, 0.9757_fp, 0.9756_fp, 0.9756_fp, 0.9755_fp, 0.9754_fp, 0.9753_fp, 0.9753_fp, &
      0.9752_fp, 0.9751_fp, 0.9751_fp, 0.9750_fp, 0.9750_fp, 0.9749_fp, 0.9749_fp, 0.9748_fp, &
      0.9747_fp, 0.9746_fp, 0.9746_fp, 0.9746_fp, 0.9745_fp, 0.9744_fp, 0.9743_fp, 0.9742_fp, &
      0.9742_fp, 0.9741_fp, 0.9740_fp, 0.9739_fp, 0.9739_fp, 0.9739_fp, 0.9738_fp, 0.9737_fp, &
      0.9736_fp, 0.9735_fp, 0.9735_fp, 0.9734_fp, 0.9733_fp, 0.9732_fp, 0.9731_fp, 0.9731_fp, &
      0.9730_fp, 0.9729_fp, 0.9728_fp, 0.9727_fp, 0.9726_fp, 0.9725_fp, 0.9724_fp, 0.9723_fp, &
      0.9723_fp, 0.9722_fp, 0.9721_fp, 0.9720_fp, 0.9719_fp, 0.9718_fp, 0.9717_fp, 0.9716_fp, &
      0.9715_fp, 0.9714_fp, 0.9713_fp, 0.9712_fp, 0.9711_fp, 0.9709_fp, 0.9708_fp, 0.9706_fp, &
      0.9705_fp, 0.9704_fp, 0.9703_fp, 0.9702_fp, 0.9700_fp, 0.9699_fp, 0.9698_fp, 0.9696_fp, &
      0.9695_fp, 0.9693_fp, 0.9691_fp, 0.9690_fp, 0.9688_fp, 0.9686_fp, 0.9685_fp, 0.9683_fp, &
      0.9682_fp, 0.9681_fp, 0.9679_fp, 0.9677_fp, 0.9676_fp, 0.9674_fp, 0.9671_fp, 0.9669_fp/

  DATA snow_stdv / 0.015_fp /
  DATA snow_em / &
      0.9716_fp, 0.9716_fp, 0.9716_fp, 0.9716_fp, 0.9713_fp, 0.9710_fp, 0.9708_fp, 0.9706_fp, &
      0.9705_fp, 0.9705_fp, 0.9705_fp, 0.9703_fp, 0.9701_fp, 0.9700_fp, 0.9699_fp, 0.9700_fp, &
      0.9702_fp, 0.9703_fp, 0.9705_fp, 0.9707_fp, 0.9710_fp, 0.9714_fp, 0.9717_fp, 0.9722_fp, &
      0.9728_fp, 0.9734_fp, 0.9740_fp, 0.9746_fp, 0.9753_fp, 0.9759_fp, 0.9765_fp, 0.9771_fp, &
      0.9778_fp, 0.9784_fp, 0.9792_fp, 0.9798_fp, 0.9806_fp, 0.9814_fp, 0.9824_fp, 0.9833_fp, &
      0.9842_fp, 0.9852_fp, 0.9863_fp, 0.9873_fp, 0.9882_fp, 0.9891_fp, 0.9901_fp, 0.9908_fp, &
      0.9914_fp, 0.9920_fp, 0.9925_fp, 0.9926_fp, 0.9928_fp, 0.9927_fp, 0.9926_fp, 0.9926_fp, &
      0.9923_fp, 0.9920_fp, 0.9918_fp, 0.9916_fp, 0.9915_fp, 0.9913_fp, 0.9911_fp, 0.9907_fp, &
      0.9905_fp, 0.9903_fp, 0.9902_fp, 0.9900_fp, 0.9897_fp, 0.9896_fp, 0.9894_fp, 0.9892_fp, &
      0.9890_fp, 0.9889_fp, 0.9886_fp, 0.9884_fp, 0.9883_fp, 0.9884_fp, 0.9885_fp, 0.9885_fp, &
      0.9884_fp, 0.9883_fp, 0.9881_fp, 0.9880_fp, 0.9880_fp, 0.9880_fp, 0.9880_fp, 0.9879_fp, &
      0.9879_fp, 0.9879_fp, 0.9879_fp, 0.9879_fp, 0.9879_fp, 0.9879_fp, 0.9878_fp, 0.9877_fp, &
      0.9876_fp, 0.9876_fp, 0.9877_fp, 0.9876_fp, 0.9875_fp, 0.9874_fp, 0.9873_fp, 0.9873_fp, &
      0.9873_fp, 0.9874_fp, 0.9875_fp, 0.9875_fp, 0.9874_fp, 0.9874_fp, 0.9874_fp, 0.9874_fp, &
      0.9874_fp, 0.9874_fp, 0.9874_fp, 0.9874_fp, 0.9874_fp, 0.9874_fp, 0.9874_fp, 0.9873_fp, &
      0.9873_fp, 0.9873_fp, 0.9873_fp, 0.9873_fp, 0.9873_fp, 0.9872_fp, 0.9871_fp, 0.9872_fp, &
      0.9871_fp, 0.9870_fp, 0.9870_fp, 0.9870_fp, 0.9870_fp, 0.9869_fp, 0.9868_fp, 0.9868_fp, &
      0.9867_fp, 0.9866_fp, 0.9866_fp, 0.9865_fp, 0.9865_fp, 0.9865_fp, 0.9866_fp, 0.9866_fp, &
      0.9865_fp, 0.9865_fp, 0.9865_fp, 0.9866_fp, 0.9866_fp, 0.9866_fp, 0.9866_fp, 0.9867_fp, &
      0.9868_fp, 0.9868_fp, 0.9868_fp, 0.9867_fp, 0.9867_fp, 0.9867_fp, 0.9866_fp, 0.9867_fp, &
      0.9867_fp, 0.9867_fp, 0.9867_fp, 0.9867_fp, 0.9867_fp, 0.9868_fp, 0.9868_fp, 0.9868_fp, &
      0.9869_fp, 0.9869_fp, 0.9870_fp, 0.9872_fp, 0.9873_fp, 0.9874_fp, 0.9874_fp, 0.9874_fp, &
      0.9875_fp, 0.9875_fp, 0.9875_fp, 0.9875_fp, 0.9876_fp, 0.9876_fp, 0.9876_fp, 0.9876_fp, &
      0.9877_fp, 0.9877_fp, 0.9877_fp, 0.9877_fp, 0.9878_fp, 0.9879_fp, 0.9879_fp, 0.9879_fp, &
      0.9878_fp, 0.9878_fp, 0.9878_fp, 0.9879_fp, 0.9879_fp, 0.9879_fp, 0.9879_fp, 0.9878_fp, &
      0.9877_fp, 0.9876_fp, 0.9876_fp, 0.9877_fp, 0.9877_fp, 0.9876_fp, 0.9876_fp, 0.9876_fp, &
      0.9876_fp, 0.9876_fp, 0.9876_fp, 0.9875_fp, 0.9874_fp, 0.9873_fp, 0.9873_fp, 0.9872_fp, &
      0.9870_fp, 0.9869_fp, 0.9869_fp, 0.9869_fp, 0.9869_fp, 0.9869_fp, 0.9868_fp, 0.9867_fp, &
      0.9867_fp, 0.9866_fp, 0.9866_fp, 0.9865_fp, 0.9865_fp, 0.9865_fp, 0.9864_fp, 0.9863_fp, &
      0.9862_fp, 0.9862_fp, 0.9862_fp, 0.9862_fp, 0.9862_fp, 0.9861_fp, 0.9860_fp, 0.9860_fp, &
      0.9860_fp, 0.9859_fp, 0.9859_fp, 0.9859_fp, 0.9858_fp, 0.9858_fp, 0.9857_fp, 0.9857_fp, &
      0.9857_fp, 0.9856_fp, 0.9855_fp, 0.9855_fp, 0.9854_fp, 0.9854_fp, 0.9853_fp, 0.9853_fp, &
      0.9852_fp, 0.9852_fp, 0.9852_fp, 0.9852_fp, 0.9852_fp, 0.9852_fp, 0.9851_fp, 0.9850_fp, &
      0.9850_fp, 0.9850_fp, 0.9850_fp, 0.9851_fp, 0.9850_fp, 0.9850_fp, 0.9849_fp, 0.9849_fp, &
      0.9850_fp, 0.9851_fp, 0.9851_fp, 0.9850_fp, 0.9850_fp, 0.9849_fp, 0.9849_fp, 0.9849_fp, &
      0.9849_fp, 0.9848_fp, 0.9848_fp, 0.9848_fp, 0.9849_fp, 0.9849_fp, 0.9849_fp, 0.9849_fp, &
      0.9849_fp, 0.9849_fp, 0.9849_fp, 0.9848_fp, 0.9848_fp, 0.9849_fp, 0.9849_fp, 0.9850_fp, &
      0.9850_fp, 0.9850_fp, 0.9851_fp, 0.9851_fp, 0.9852_fp, 0.9852_fp, 0.9853_fp, 0.9853_fp, &
      0.9854_fp, 0.9854_fp, 0.9854_fp, 0.9854_fp, 0.9854_fp, 0.9854_fp, 0.9854_fp, 0.9854_fp, &
      0.9855_fp, 0.9856_fp, 0.9856_fp, 0.9856_fp, 0.9856_fp, 0.9856_fp, 0.9856_fp, 0.9856_fp, &
      0.9856_fp, 0.9855_fp, 0.9855_fp, 0.9855_fp, 0.9854_fp, 0.9853_fp, 0.9853_fp, 0.9853_fp, &
      0.9853_fp, 0.9853_fp, 0.9853_fp, 0.9853_fp, 0.9853_fp, 0.9853_fp, 0.9853_fp, 0.9853_fp, &
      0.9852_fp, 0.9851_fp, 0.9851_fp, 0.9850_fp, 0.9849_fp, 0.9849_fp, 0.9849_fp, 0.9848_fp, &
      0.9848_fp, 0.9848_fp, 0.9848_fp, 0.9848_fp, 0.9848_fp, 0.9848_fp, 0.9847_fp, 0.9846_fp, &
      0.9846_fp, 0.9846_fp, 0.9847_fp, 0.9846_fp, 0.9845_fp, 0.9844_fp, 0.9844_fp, 0.9843_fp, &
      0.9842_fp, 0.9842_fp, 0.9842_fp, 0.9842_fp, 0.9841_fp, 0.9841_fp, 0.9840_fp, 0.9839_fp, &
      0.9838_fp, 0.9838_fp, 0.9837_fp, 0.9837_fp, 0.9837_fp, 0.9836_fp, 0.9836_fp, 0.9835_fp, &
      0.9835_fp, 0.9834_fp, 0.9833_fp, 0.9832_fp, 0.9832_fp, 0.9832_fp, 0.9831_fp, 0.9831_fp, &
      0.9830_fp, 0.9829_fp, 0.9828_fp, 0.9828_fp, 0.9827_fp, 0.9827_fp, 0.9826_fp, 0.9826_fp, &
      0.9825_fp, 0.9824_fp, 0.9824_fp, 0.9823_fp, 0.9821_fp, 0.9821_fp, 0.9820_fp, 0.9821_fp, &
      0.9820_fp, 0.9820_fp, 0.9818_fp, 0.9817_fp, 0.9817_fp, 0.9816_fp, 0.9815_fp, 0.9815_fp, &
      0.9815_fp, 0.9814_fp, 0.9813_fp, 0.9813_fp, 0.9812_fp, 0.9811_fp, 0.9811_fp, 0.9810_fp/  

CONTAINS

!------------------------------------------
! Routines for initialising database
!------------------------------------------
  
   FUNCTION crtm_uwiremis_init( &
        &             path,       &! in
        &             imonth )     &! in
    RESULT( Error_Status )
         
          
    ! Description:
    ! initialize the rttov_uwiremis algorithm by (1) reading in the UW BF IR Global 
    ! Emissivty data and (2) the eigenvectors of the laboratory spectra, and (2) make some
    ! precalculations for the PCA regression
    ! 
    ! History:
    ! Version   Date     Comment
    ! -------   ----     -------
    !  0.9    03/31/2009   origianl code E. Borbas UW-Madison/CIMSS 
    !  1.0    03/31/2009  New F90 code with structures (E Borbas B Ruston)
    !
             
!    Implicit None

    Character (len=*),  Intent(in)  :: path
    Integer, Intent(in)  :: imonth
    ! Function result
    INTEGER :: Error_Status
    CHARACTER(256) :: Message
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'crtm_uwiremis_init'
 
    
    Integer :: i,info
    Integer :: ipiv(numpcs)
    Integer, Parameter :: nmonth=12

    Real(fp)  :: A(numpcs,hngpnts),AT(hngpnts,numpcs)
    Double Precision :: B(numpcs,numpcs),C(numpcs,numpcs)
    
    Character (len=300) :: fn
    Character (len=4) :: cyear
    Character (len=2) :: cmonth

    Logical :: file_exists
    
    Error_Status = SUCCESS

    Write(cyear,'(i4)') db_ver_year
    Write(cmonth,'(i2.2)') imonth

    Allocate(bfemis_flag  (nb_lats,nb_lons))
    Allocate(bfemis_lut   (nb_lats,nb_lons))
    Allocate(bfemis       (nb_pack,hngpnts))
    
    Allocate(cov_emis_lut (cv_lats,cv_lons))
    Allocate(cov_emis     (cv_pack,numwave))
      
    !----------------------------------------------------------------------------
    ! reading the 0.1 degree resolution UW BF IR Land Surface Global Emissivty 
    !----------------------------------------------------------------------------
    fn=path//'UWirbfemis_V2.1_0.1deg_'//cyear//cmonth//'_mask.nc'
    Inquire(FILE=fn, EXIST=file_exists)
    If (.not. file_exists) Then
      Error_Status = FAILURE  
      WRITE( Message,'("UWiremis means file ",a,&
                     &" not found")') TRIM(fn) 
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      Return
    End If
 
    Call crtm_uwiremis_read_db(Trim(fn))
    
    !----------------------------------------------------------------------------
    ! reading the 0.5 degree resolution UW IR Land Surface Global Emissivty STD DEV
    !----------------------------------------------------------------------------
    fn=path//'UWiremis_hsremis_covmat_V1.0_deg0.5_month'//cmonth//'_mask.nc'
    Inquire(FILE=fn, EXIST=file_exists)
    If (.not. file_exists) Then
      Error_Status = FAILURE  
      WRITE( Message,'("UWiremis covariances  file ",a,&
                     &" not found")') TRIM(fn) 
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      Return
    End If

    Call crtm_uwiremis_read_cov(Trim(fn))
          
    !-------------------------------------------------------------------
    !  reading the eigienvectors of the 128 selected laboratory spectra 
    !-------------------------------------------------------------------
    fn=path//'UWiremis_labeigvects.nc'
    Inquire(FILE=fn, EXIST=file_exists)
    If (.not. file_exists) Then
      Error_Status = FAILURE  
      WRITE( Message,'("UWiremis laboratory spectra file ",a,&
                     &" not found")') TRIM(fn) 
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      Return
    End If

    Call crtm_uwiremis_read_labeigvects(Trim(fn))
          
    !---------------------------------------------------
    ! compute D matrix for HSR emissivity construction
    !---------------------------------------------------
  
    ! define matrix A
    
    A=pcu_modres(1:numpcs,1:hngpnts)
    
    ! calculate transpose matrix A : AT [6X4]         
    
    AT=transpose(A) 
  
    !  B=A*A' [4X6]X[6X4]   first: row second: column
  
    B(:,:)=0.0_fp
    B=MATMUL(A,AT)
  
    ! computing inverse of B
  
    C(:,:)=0.0_fp
    Do i=1,numpcs
      C(i,i)=1.0_fp
    Enddo
    
    Call DGESV( numpcs, numpcs, B, numpcs, IPIV, C, numpcs, INFO )
                  
    ! compute D=A'*inv(A*A')=A'*inv(B)= AT * C [6X4] * [4X4]
  
    D=MATMUL(AT,C)
          
  END FUNCTION crtm_uwiremis_init
  
  
  SUBROUTINE  crtm_uwiremis_read_cov( &
        &                          fn) !in
  
    ! Description:
    ! read the 0.1 degree resolution UW BF IR Global Emissivty data   
    ! from the netCDF file into memory. 
    ! 
    ! History:
    ! Version   Date     Comment
    ! -------   ----     -------
    !  1.0    03/31/2009   origianl code B. Ruston 
    !
               
    Implicit None
  
    ! Define Variables.
  
    Character(len=*), Intent(in) :: fn    ! filename including full path?
  

    Integer(Kind=ncint32) :: nvars     ! number of variables
    Integer(Kind=ncint32) :: ndims     ! number of dimensions
    Integer(Kind=ncint32) :: errstat   ! error code
    Integer(Kind=ncint32) :: recdim    ! record dimension
    Integer(Kind=ncint32) :: nc_dim(4)
  
    Integer    :: i, j
    Integer(Kind=ncint32) :: ncid, ngatts, nrecs, varid
  
    Real(Kind=ncreal32)   ::  sfac
    Integer(Kind=ncint16), Allocatable, Dimension(:,:) :: emis_cov
    Integer(Kind=ncint8),  Allocatable, Dimension(:,:) :: pack_cov
    
    Character (len=1024) :: strbuf ! string buffer for var
    Integer   :: indexlut
    
  
    ! Open netCDF file.
    errstat = nf_open(Trim(fn),nf_nowrite,ncid)
  
    ! Get info on the record dimension for this file.
    errstat = nf_inq(ncid,ndims,nvars,ngatts,recdim)
  
    Do recdim=1,ndims
      errstat = nf_inq_dim(ncid,recdim,strbuf,nrecs)
      nc_dim(recdim) = nrecs
    End Do
        
    ! Retrieve database of diagonal of covariance of MODIS emissivity values
    Allocate(pack_cov(nc_dim(2),nc_dim(3)))
    errstat = nf_inq_varid (ncid,'mask',varid)
    errstat = nf_get_var_int1(ncid,varid,pack_cov)
  
    ! Generate the look-up table into the covariance data
    cov_emis_lut(:,:) = -1
    indexlut = 1
    Do i=1,nc_dim(3)
      Do j=1,nc_dim(2)
        If (pack_cov(j,i) > 0) then
          cov_emis_lut(j,i) = indexlut
          indexlut = indexlut + 1
        End If
      End Do
    End Do
  
    ! Retrieve database of diagonal of covariance of MODIS emissivity values
    Allocate(emis_cov(nc_dim(1),nc_dim(4)))
    errstat = nf_inq_varid (ncid,'emis_diagCov',varid)
    errstat = nf_get_var_int2(ncid,varid,emis_cov)
    errstat = nf_get_att_real(ncid,varid,'scale_factor',sfac)
  
    Do i=1,nc_dim(1)
      cov_emis(:,i) = Real(emis_cov(i,:),fp)*sfac 
    End Do
  
    Deallocate(pack_cov,emis_cov)
          
    errstat = nf_close(ncid)

  END SUBROUTINE crtm_uwiremis_read_cov
  
  
  SUBROUTINE  crtm_uwiremis_read_db( &
        &                          fn) !in
  
    ! Description:
    ! read the 0.1 degree resolution UW BF IR Global Emissivty data   
    ! from the netCDF file into memory. 
    ! 
    ! History:
    ! Version   Date     Comment
    ! -------   ----     -------
    !  1.0    03/31/2009   origianl code B. Ruston 
    !
   
    Implicit None
  
    ! Define Variables.
  
    Character(len=*), Intent(in) :: fn    ! filename including full path?


    Integer(Kind=ncint32) :: nvars     ! number of variables
    Integer(Kind=ncint32) :: ndims     ! number of dimensions
    Integer(Kind=ncint32) :: errstat   ! error code
    Integer(Kind=ncint32) :: recdim    ! record dimension
    Integer(Kind=ncint32) :: nc_dim(4) ! hng_pnt, lats, lons, pack_len
  
    Integer    :: i,j,k
    Integer(Kind=ncint32) :: ncid, ngatts, nrecs, varid
  
    Real(Kind=ncreal32)   :: sfac, offs
    Integer(Kind=ncint16), Allocatable, Dimension(:) :: emis_ch
  
    Character (len=1024) :: strbuf ! string buffer for var
    Character (len=6)    :: cfld 
    Integer   :: indexlut

    ! Open netCDF file.
    errstat = nf_open(Trim(fn),nf_nowrite,ncid)
    
    ! Get info on the record dimension for this file. 
    errstat = nf_inq(ncid,ndims,nvars,ngatts,recdim)
    
    Do recdim=1,ndims
      errstat = nf_inq_dim(ncid,recdim,strbuf,nrecs)
      nc_dim(recdim) = nrecs
    End Do
  
    Allocate(emis_ch(nc_dim(4)))
  
    ! Retrieve emissivity database flag value
    errstat = nf_inq_varid (ncid, 'emis_flag', varid)
    errstat = nf_get_var_int2(ncid,varid,bfemis_flag)
  
    ! Generate the look-up table into the emissivity data
    bfemis_lut(:,:) = -1
    indexlut = 1
    Do i=1,nc_dim(3)
      Do j=1,nc_dim(2)
        If (bfemis_flag(j,i) > 0) then
          bfemis_lut(j,i) = indexlut
          indexlut = indexlut + 1
        End If
      End Do
    End Do
  
    ! Retrieve database of 10 MODIS hinge-point emissivity values
    Do k=1,nc_dim(1)
      If (k < 10) Then
        write(cfld,'("emis",i1," ")') k
      Else
        write(cfld,'("emis",i2.2)') k
      End if
      errstat = nf_inq_varid (ncid,cfld,varid)
      errstat = nf_get_var_int2(ncid,varid,emis_ch)
      errstat = nf_get_att_real(ncid,varid,'scale_factor',sfac)
      errstat = nf_get_att_real(ncid,varid,'add_offset',offs)

      bfemis(:,11-k) = Real(emis_ch,fp)*sfac + offs
    End Do
  
    Deallocate(emis_ch)

    errstat = nf_close(ncid)
  
  END SUBROUTINE crtm_uwiremis_read_db
  
  
  SUBROUTINE  crtm_uwiremis_read_labeigvects( &
        &                                   fn) !in
  
    ! Description:
    ! read the eigienvectors of the 128 selected laboratory spectra 
    !(created by E borbas) from the netCDF file into memory. 
    ! 
    ! History:
    ! Version   Date     Comment
    ! -------   ----     -------
    !  1.0    03/31/2009   origianl code B. Ruston 
    !
   
    Implicit None
  
    ! Define Variables.
    Character(len=*), Intent(in) :: fn    ! filename including full path?
  

    Integer(Kind=ncint32) :: nvars     ! number of variables
    Integer(Kind=ncint32) :: ndims     ! parameter (ndims = 1) ! number of dimensions
    Integer(Kind=ncint32) :: errstat   ! error code
    Integer(Kind=ncint32) :: recdim    ! record dimension
    Integer(Kind=ncint32) :: xy_dim(2)
  
    Integer(Kind=ncint32) :: ncid, ngatts, nrecs, varid
  
    Character (len=1024)  :: strbuf  ! string buffer for var
  
  
    ! Open netCDF file.
    errstat = nf_open(Trim(fn),nf_nowrite,ncid)
  
    ! Get info on the record dimension for this file.
    errstat = nf_inq(ncid,ndims,nvars,ngatts,recdim)
    
    Do recdim=1,ndims
      errstat = nf_inq_dim(ncid,recdim,strbuf,nrecs)
      xy_dim(recdim) = nrecs
    End Do
  
    ! Retrieve the laboratory eigenvalues into array pcu 
    errstat = nf_inq_varid (ncid, 'PC_scores', varid)
    errstat = nf_get_var_real(ncid,varid,pcu)

    errstat = nf_close(ncid)
  
  END SUBROUTINE crtm_uwiremis_read_labeigvects
  
  
   
!------------------------------------------
! Routines for returning emissivity values
!------------------------------------------
  
  SUBROUTINE crtm_uwiremis( &
        & nchs,              &! in 
        & lat,               &! in 
        & lon,               &! in
        & surfacetype,       &! in
        & snowfrac,          &! in 
        & instr_wavenum,     &! in 
        & instr_emis,        &! out 
        & instr_emis_cov,    &! out 
        & instr_emis_flag)    ! out 
  
    ! Description:
    ! To compute IR emissivty for a given location and frequency 
    ! from the 0.1 degree resolution UW BF IR Global Emissivty data 
    ! (at 10 hinge points) (http://cimss.ssec.wisc.edu/iremis/)
    ! and labratory measurements using principal component analyses
    !
    !
    ! History:
    ! Version   Date     Comment
    ! -------   ----     -------
    !  0.9       03/31/2009  Original code E Borbas UW-Madison/CIMSS
    !  1.0       03/31/2009  New F90 code with structures (E Borbas B Ruston)
   
    Integer, intent(in)  :: nchs
    Integer, intent(in)  :: surfacetype
    Integer, Intent(out) :: instr_emis_flag
  
    Real(fp),    Intent(in)  :: lat, lon
    Real(fp),    Intent(in)  :: snowfrac
    Real(fp),    Intent(in)  :: instr_wavenum(nchs)
    Real(fp),    Intent(out) :: instr_emis(nchs)
    Real(fp),    Intent(out) :: instr_emis_cov(nchs)
  

    Real(fp) :: bfem(hngpnts)
    Real(fp) :: hsremis(numwave)
    Real(fp) :: emis_cov(numwave)
    
    Real(fp) :: long
    Integer :: gridy, gridx, rnd_x, rnd_y
          
    Real(fp) :: hkod = -999._fp
    
    Real(fp) :: hsr_ir_emis(nchs)
    Real(fp) :: hsr_ir_emis_cov(nchs)
    Real(fp) :: cov_buff(numwave)
    
    instr_emis(:) = hkod  
        
    If ( surfacetype == surftype_land ) Then
        
    !------------------------------------------------------------
    ! find the closest grid point from the uwiremis database
    !------------------------------------------------------------
      
      long = modulo(lon,360.0_fp)
      If (long > 180.0) Then
        long = long - 360.0_fp
      End If
    
      gridy = Int(Abs(bfemis_ygrid1-lat)/bfemis_gridres)+1
      gridx = Int(Abs(bfemis_xgrid1-long)/bfemis_gridres)+1
                                      
      rnd_y = Int(Abs(cov_emis_ygrid1-lat)/cov_emis_gridres)+1
      rnd_x = Int(Abs(cov_emis_xgrid1-long)/cov_emis_gridres)+1
                                      
      instr_emis_flag = bfemis_flag(gridy,gridx)

    !------------------------------
    ! check if it is a land pixel
    !------------------------------ 
            
      If ( instr_emis_flag > 0 ) Then
        
        ! Find the emissivity and covariances
        
        If (bfemis_lut(gridy,gridx) > 0) Then
          bfem(:) = Real(bfemis(bfemis_lut(gridy,gridx),:),fp)
        Else
          bfem(:) = 0.0_fp
        End If
        If (cov_emis_lut(rnd_y,rnd_x) > 0) Then
          emis_cov(:) = SQRT(Real(cov_emis(cov_emis_lut(rnd_y,rnd_x),:),fp))
        Else
          emis_cov(:) = default_std
        End If
          
    !--------------------------------------------------------------------------------------------------------
    ! compute the hsr emissivity spectra at 416 wavenumber points from the 10 BF emissivity hinge points
    !-------------------------------------------------------------------------------------------------------
    
        Call crtm_uwiremis_recon_hsremis( &
          & bfem,                          &! in
          & hsremis)                        ! out
      
    !--------------------------------------------------------------------------------
    ! create instrument specific emis/stdv by finidng the closest wavenumber value
    !--------------------------------------------------------------------------------
    
        Call crtm_uwiremis_select_wavenum( &
          & hsremis,                        &! in  
          & emis_cov,                       &! in  
          & nchs,                           &! in 
          & instr_wavenum(1:nchs),          &! in 
          & instr_emis,                     &! out 
          & instr_emis_cov       )           ! out 
            
    !---------------------------------------------------
    ! Linearly blend avg snow emis with snow cover frac
    !---------------------------------------------------
        If ( snowfrac > 0.0 ) Then

          ! snow_stdv is a fixed stdv
          cov_buff(:) = snow_stdv
          Call crtm_uwiremis_select_wavenum( &
                  & snow_em,                  &! in  
                  & cov_buff,                 &! in  
                  & nchs,                     &! in 
                  & instr_wavenum,            &! in 
                  & hsr_ir_emis,              &! out 
                  & hsr_ir_emis_cov       )    ! out 
      
          If ( snowfrac > 1.0 ) Then
            instr_emis(:) = hsr_ir_emis(:)
            ! Note: a stdv was passed into hsr_ir_emis_cov -- no sqrt
            instr_emis_cov(:) = hsr_ir_emis_cov(:)
          Else
            instr_emis(:) = snowfrac*hsr_ir_emis(:) + (1.0_fp-snowfrac)*instr_emis(:)
            ! Note: a stdv was passed into hsr_ir_emis_cov -- no sqrt
            instr_emis_cov(:) = snowfrac*hsr_ir_emis_cov(:) + (1.0_fp-snowfrac)*instr_emis_cov(:)
          End If
          
        End If  ! snow chk
        
      End If  ! emis flag > 0
    
    Else If ( surfacetype == surftype_seaice) Then
    
    !---------------------------------------
    ! Return emissivity and stdv for seaice
    !---------------------------------------
  
      ! sice_stdv is a fixed stdv
      cov_buff(:) = sice_stdv
      Call crtm_uwiremis_select_wavenum( &
                & sice_em,                &! in  
                & cov_buff,               &! in  
                & nchs,                   &! in 
                & instr_wavenum,          &! out 
                & instr_emis,             &! out 
                & instr_emis_cov        )  ! out 
        
      instr_emis_flag = seaice_flag
    Else
      Print *,'Warning: IR emissivity atlas should only be called for land and seaice surface types'
    End If
  END SUBROUTINE crtm_uwiremis
  
  
  SUBROUTINE crtm_uwiremis_recon_hsremis( &
        & bfem,                            &! in 
        & hsremis)                          ! out 
          
    ! Description:
    ! To creates high spectra resolution emissivties at 416 wavenumbers
    ! from the UW BF IR Global Emissivty data (at 10 hinge points) 
    ! and labratory measurements using principal component analyses
    !
    !
    ! History:
    ! Version   Date     Comment
    ! -------   ----     -------
    !  0.9       03/31/2009  Original code E Borbas UW-Madison/CIMSS
    !  1.0       03/31/2009  New F90 code with structures (E Borbas B Ruston)
    !
 
    Implicit None
  
    
    Real(fp), Intent(in)  :: bfem(hngpnts)
    Real(fp), Intent(out) :: hsremis(numwave)
  

    Integer :: j,k
  
    Real(fp) :: col(hngpnts)     
    Real(fp) :: coef(numpcs) 
    Real(fp) :: emis(numwave) 
  
    !-------------------------
    ! calculate the regcoef
    !-------------------------
  
    coef(:) = 0._fp
      
    If ( bfem(1) > 0._fp ) Then
      
      Do j=1,hngpnts
        col(j) = bfem(j) - pcm_modres(j)
      End Do
    
      Do k=1,numpcs
        Do j=1,hngpnts
          coef(k) = coef(k) + col(j) * D(j,k)
        End Do
      End Do

    !-----------------------------------
    ! apply regcoef to get the hsr dataset
    !-----------------------------------
  
      emis=0._fp
          
      If ( coef(1) .ne. -999._fp ) Then
  
        Do k=1,numwave
          Do j=1,numpcs
            emis(k) = emis(k) + coef(j) * Real(pcu(j,k),fp)
          End Do
  
          emis(k) = emis(k) + pcm(k)
        End Do
  
    !--------------------------------------------
    !  filter out emissivty values larger then 1.
    !--------------------------------------------
  
        Do j=1,numwave
          If ( emis(j) > 1._fp ) emis(j) = 1._fp                        
          hsremis(j) = emis(j)
        End Do
          
      Else
                  
        coef = -999._fp
        hsremis = -999._fp
              
      End If

    End If
                  
  END SUBROUTINE crtm_uwiremis_recon_hsremis
  
  
  SUBROUTINE crtm_uwiremis_select_wavenum ( &
        & hsremis,                           &! in
        & emis_cov,                          &! in
        & nchs,                              &! in
        & instr_wavenum,                     &! in
        & instr_emis,                        &! out
        & instr_emis_cov)                     ! out
  
    ! Description:
    ! subroutine to find the closest wavenumber from the UW HSR emissivity spectra 
    ! for the instrument frequency and assign the instrument emissivity by choosing the 
    ! closest spectral point value or bilinear interpolating  between the two 
    ! closest spectral point values
    !
    !
    ! History:
    ! Version   Date     Comment
    ! -------   ----     -------
    !  0.9       03/31/2009  Original code E Borbas UW-Madison/CIMSS
    !  1.0       03/31/2009  New F90 code with structures (E Borbas B Ruston)
               
    Integer, Intent(in) :: nchs
  
    Real(fp), Intent(in) :: hsremis(numwave)
    Real(fp), Intent(in) :: emis_cov(numwave)
    Real(fp), Intent(in) :: instr_wavenum(nchs)
  
    Real(fp), Intent(out) :: instr_emis(nchs)
    Real(fp), Intent(out) :: instr_emis_cov(nchs)
  

    Integer :: j,k
  
    Real(fp) :: dist(numwave)
    Real(fp) :: mindist(nchs)
    Integer :: ind_mindist(nchs)
          
    Real(fp) :: dwvnum1,dwvnum2,dwvsum
    Real(fp) :: hsremis1,hsremis2,emis_cov1,emis_cov2
    Logical  :: lcpu_emis, lcpu_cov
  
    Real(fp) :: hkod = -999._fp
  
    ! initialize instrument emissivity
    
    !---------------------------------------------------------------
    ! finindg the closest frequency from the hsr emissivity spectr
    !--------------------------------------------------------------
    lcpu_emis = .True.
    lcpu_cov  = .True.
    If ( All(hsremis == hsremis(1)) ) lcpu_emis = .False.
    If ( All(emis_cov == emis_cov(1)) ) lcpu_cov  = .False.
              
    If (lcpu_emis .or. lcpu_cov) Then           
      instr_emis(:)       = hkod
      instr_emis_cov(:)   = hkod
      Do j=1,nchs
      
        If(instr_wavenum(j) <= hsr_wavenum(1)) Then

          instr_emis(j)=hsremis(1)
          instr_emis_cov(j)=emis_cov(1)

        Elseif(instr_wavenum(j) >= hsr_wavenum(numwave)) Then     

          instr_emis(j)=hsremis(numwave)
          instr_emis_cov(j)=emis_cov(numwave)

        Else ! within wavenumber compute range

          mindist(j) = 100._fp
          ind_mindist(j) = 100000
                                                  
          Do k=1,numwave
  
            ! calucalte distances between the instr freq end hsr emissivities                             
            dist(k) = abs( instr_wavenum(j) - hsr_wavenum(k) )
        
            ! finding the closest frequency from the hsr emissivity 
            If( dist(k) <=  mindist(j) ) Then
        
              mindist(j) = dist(k)
              ind_mindist(j) = k
        
            End If
          End do
  
    !--------------------------------
    ! assign instrument emissivity
    !-------------------------------- 
    !  closest spectral point
    !                       instr_emis(j)=hsremis(ind_mindist(j))
    !                       instr_emis_cov(j)=cov_emis(ind_mindist(j))
    
    ! or bilinear mean of the two closest spectral points
  
          k=1 
          dwvnum1=0._fp
          dwvnum2=0._fp
          hsremis1=0._fp
          hsremis2=0._fp
          emis_cov1=0._fp
          emis_cov2=0._fp

          If ( instr_wavenum(j) <= hsr_wavenum(ind_mindist(j)) ) k=-1

            dwvnum1 = dist( ind_mindist(j) )
            dwvnum2 = dist( ind_mindist(j) + k )
            dwvsum = dwvnum1 + dwvnum2

          If ( lcpu_emis ) Then
            hsremis1 = dwvnum1 * hsremis( ind_mindist(j) + k )
            hsremis2 = dwvnum2 * hsremis( ind_mindist(j) )
            instr_emis(j) = ( hsremis1 + hsremis2 ) / dwvsum
          Else
            instr_emis(j) = hsremis(1)
          End If

          If ( lcpu_cov ) Then
            emis_cov1 = dwvnum1 * emis_cov( ind_mindist(j) + k )
            emis_cov2 = dwvnum2 * emis_cov( ind_mindist(j) )
            instr_emis_cov(j) = ( emis_cov1 + emis_cov2 ) / dwvsum
          Else
            instr_emis_cov(j) = emis_cov(1)
          End If
  
        End If    !==  (instr_wavenum(j) <= hsr_wavenum(1)) 
  
      End Do
  
    Else  ! all logical computes (lcpu_emis, lcpu_cov) are false
      instr_emis(:)=hsremis(1)
      instr_emis_cov(:)=emis_cov(1)
    End If
  
  END SUBROUTINE crtm_uwiremis_select_wavenum
  
  
!------------------------------------
! Routine to deallocate atlas arrays
!------------------------------------
  SUBROUTINE crtm_uwiremis_close_atlas
    IF ( ALLOCATED(bfemis_flag)  ) DEALLOCATE(bfemis_flag)
    IF ( ALLOCATED(bfemis_lut)   ) DEALLOCATE(bfemis_lut)
    IF ( ALLOCATED(bfemis)       ) DEALLOCATE(bfemis)
    IF ( ALLOCATED(cov_emis_lut) ) DEALLOCATE(cov_emis_lut)
    IF ( ALLOCATED(cov_emis)     ) DEALLOCATE(cov_emis)
  END SUBROUTINE
  
END MODULE mod_iratlas
