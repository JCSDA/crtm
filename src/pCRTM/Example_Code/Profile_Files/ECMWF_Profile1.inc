  CHARACTER( * ), PARAMETER :: PROFILE_NAME = 'ECMWF Profile #1'

  INTEGER, PARAMETER :: N_LAYERS   = 100  ! K dimension
  INTEGER, PARAMETER :: N_PROFILES =   1  ! M dimension

  ! -- Some profile data
  REAL( fp_kind ), DIMENSION( N_LAYERS, N_PROFILES ) :: LEVEL_PRESSURE = RESHAPE( &
    (/   0.01606_fp_kind,    0.03838_fp_kind,    0.07688_fp_kind,    0.13695_fp_kind,    0.22441_fp_kind, &
         0.34540_fp_kind,    0.50637_fp_kind,    0.71402_fp_kind,    0.97527_fp_kind,    1.29724_fp_kind, &
         1.68720_fp_kind,    2.15257_fp_kind,    2.70090_fp_kind,    3.33981_fp_kind,    4.07704_fp_kind, &
         4.92036_fp_kind,    5.87762_fp_kind,    6.95669_fp_kind,    8.16548_fp_kind,    9.51189_fp_kind, &
        11.00384_fp_kind,   12.64922_fp_kind,   14.45594_fp_kind,   16.43183_fp_kind,   18.58473_fp_kind, &
        20.92241_fp_kind,   23.45258_fp_kind,   26.18292_fp_kind,   29.12101_fp_kind,   32.27438_fp_kind, &
        35.65047_fp_kind,   39.25663_fp_kind,   43.10014_fp_kind,   47.18817_fp_kind,   51.52779_fp_kind, &
        56.12595_fp_kind,   60.98953_fp_kind,   66.12526_fp_kind,   71.53977_fp_kind,   77.23956_fp_kind, &
        83.23102_fp_kind,   89.52039_fp_kind,   96.11380_fp_kind,  103.01724_fp_kind,  110.23656_fp_kind, &
       117.77748_fp_kind,  125.64556_fp_kind,  133.84624_fp_kind,  142.38480_fp_kind,  151.26637_fp_kind, &
       160.49594_fp_kind,  170.07835_fp_kind,  180.01828_fp_kind,  190.32026_fp_kind,  200.98866_fp_kind, &
       212.02771_fp_kind,  223.44146_fp_kind,  235.23381_fp_kind,  247.40851_fp_kind,  259.96914_fp_kind, &
       272.91912_fp_kind,  286.26171_fp_kind,  300.00000_fp_kind,  314.13694_fp_kind,  328.67529_fp_kind, &
       343.61766_fp_kind,  358.96650_fp_kind,  374.72410_fp_kind,  390.89257_fp_kind,  407.47386_fp_kind, &
       424.46978_fp_kind,  441.88194_fp_kind,  459.71182_fp_kind,  477.96072_fp_kind,  496.62979_fp_kind, &
       515.71999_fp_kind,  535.23215_fp_kind,  555.16693_fp_kind,  575.52483_fp_kind,  596.30618_fp_kind, &
       617.51116_fp_kind,  639.13980_fp_kind,  661.19195_fp_kind,  683.66732_fp_kind,  706.56546_fp_kind, &
       729.88577_fp_kind,  753.62749_fp_kind,  777.78972_fp_kind,  802.37138_fp_kind,  827.37126_fp_kind, &
       852.78800_fp_kind,  878.62010_fp_kind,  904.86588_fp_kind,  931.52355_fp_kind,  958.59115_fp_kind, &
       986.06660_fp_kind, 1013.94766_fp_kind, 1042.23194_fp_kind, 1070.91694_fp_kind, 1100.00000_fp_kind /), &
    (/ N_LAYERS, N_PROFILES /) )

  REAL( fp_kind ), DIMENSION( N_LAYERS, N_PROFILES ) :: LAYER_PRESSURE = RESHAPE( &
    (/   0.00948_fp_kind,    0.02562_fp_kind,    0.05542_fp_kind,    0.10404_fp_kind,    0.17710_fp_kind, &
         0.28057_fp_kind,    0.42077_fp_kind,    0.60426_fp_kind,    0.83787_fp_kind,    1.12861_fp_kind, &
         1.48369_fp_kind,    1.91045_fp_kind,    2.41638_fp_kind,    3.00906_fp_kind,    3.69618_fp_kind, &
         4.48549_fp_kind,    5.38482_fp_kind,    6.40201_fp_kind,    7.54496_fp_kind,    8.82157_fp_kind, &
        10.23975_fp_kind,   11.80743_fp_kind,   13.53248_fp_kind,   15.42280_fp_kind,   17.48620_fp_kind, &
        19.73049_fp_kind,   22.16343_fp_kind,   24.79270_fp_kind,   27.62593_fp_kind,   30.67068_fp_kind, &
        33.93444_fp_kind,   37.42460_fp_kind,   41.14848_fp_kind,   45.11329_fp_kind,   49.32617_fp_kind, &
        53.79412_fp_kind,   58.52406_fp_kind,   63.52280_fp_kind,   68.79701_fp_kind,   74.35326_fp_kind, &
        80.19799_fp_kind,   86.33753_fp_kind,   92.77805_fp_kind,   99.52562_fp_kind,  106.58616_fp_kind, &
       113.96545_fp_kind,  121.66912_fp_kind,  129.70270_fp_kind,  138.07152_fp_kind,  146.78080_fp_kind, &
       155.83560_fp_kind,  165.24084_fp_kind,  175.00127_fp_kind,  185.12150_fp_kind,  195.60598_fp_kind, &
       206.45900_fp_kind,  217.68472_fp_kind,  229.28710_fp_kind,  241.26997_fp_kind,  253.63699_fp_kind, &
       266.39167_fp_kind,  279.53734_fp_kind,  293.07719_fp_kind,  307.01422_fp_kind,  321.35130_fp_kind, &
       336.09111_fp_kind,  351.23619_fp_kind,  366.78889_fp_kind,  382.75142_fp_kind,  399.12581_fp_kind, &
       415.91394_fp_kind,  433.11753_fp_kind,  450.73811_fp_kind,  468.77707_fp_kind,  487.23564_fp_kind, &
       506.11488_fp_kind,  525.41569_fp_kind,  545.13880_fp_kind,  565.28479_fp_kind,  585.85408_fp_kind, &
       606.84693_fp_kind,  628.26343_fp_kind,  650.10354_fp_kind,  672.36702_fp_kind,  695.05353_fp_kind, &
       718.16251_fp_kind,  741.69330_fp_kind,  765.64506_fp_kind,  790.01681_fp_kind,  814.80740_fp_kind, &
       840.01554_fp_kind,  865.63981_fp_kind,  891.67861_fp_kind,  918.13022_fp_kind,  944.99274_fp_kind, &
       972.26418_fp_kind,  999.94235_fp_kind, 1028.02495_fp_kind, 1056.50954_fp_kind, 1085.39353_fp_kind /), &
    (/ N_LAYERS, N_PROFILES /) )

  REAL( fp_kind ), DIMENSION( N_LAYERS, N_PROFILES ) :: LAYER_TEMPERATURE = RESHAPE( &
    (/ 229.10850_fp_kind,  226.97900_fp_kind,  235.29050_fp_kind,  239.31450_fp_kind,  243.87350_fp_kind, &
       250.32300_fp_kind,  256.56300_fp_kind,  262.18150_fp_kind,  266.53650_fp_kind,  269.60850_fp_kind, &
       270.20350_fp_kind,  264.52650_fp_kind,  251.57800_fp_kind,  240.26350_fp_kind,  235.09450_fp_kind, &
       232.95950_fp_kind,  233.01700_fp_kind,  233.89650_fp_kind,  234.38550_fp_kind,  233.68150_fp_kind, &
       232.43650_fp_kind,  231.60750_fp_kind,  231.19250_fp_kind,  230.80800_fp_kind,  230.08800_fp_kind, &
       228.60300_fp_kind,  226.40700_fp_kind,  223.65450_fp_kind,  220.52500_fp_kind,  218.22550_fp_kind, &
       216.66850_fp_kind,  215.10700_fp_kind,  213.53750_fp_kind,  212.00550_fp_kind,  210.50700_fp_kind, &
       208.88300_fp_kind,  206.79350_fp_kind,  204.41500_fp_kind,  202.05850_fp_kind,  199.71800_fp_kind, &
       197.66800_fp_kind,  196.16900_fp_kind,  194.99350_fp_kind,  194.83500_fp_kind,  195.64800_fp_kind, &
       196.87850_fp_kind,  198.83050_fp_kind,  201.09100_fp_kind,  203.55800_fp_kind,  206.18950_fp_kind, &
       208.90000_fp_kind,  211.73600_fp_kind,  214.60150_fp_kind,  217.52200_fp_kind,  220.45650_fp_kind, &
       223.33400_fp_kind,  226.15650_fp_kind,  228.90050_fp_kind,  231.55700_fp_kind,  234.17250_fp_kind, &
       236.78800_fp_kind,  239.40950_fp_kind,  242.14000_fp_kind,  244.95300_fp_kind,  247.79300_fp_kind, &
       250.66450_fp_kind,  253.21650_fp_kind,  255.36700_fp_kind,  257.01750_fp_kind,  258.03400_fp_kind, &
       258.77800_fp_kind,  259.45450_fp_kind,  260.22500_fp_kind,  261.25100_fp_kind,  262.67150_fp_kind, &
       264.61350_fp_kind,  266.85350_fp_kind,  269.15900_fp_kind,  271.44850_fp_kind,  273.67250_fp_kind, &
       275.95500_fp_kind,  278.34150_fp_kind,  280.82250_fp_kind,  283.34850_fp_kind,  285.82600_fp_kind, &
       288.28800_fp_kind,  290.72100_fp_kind,  293.13450_fp_kind,  295.60900_fp_kind,  298.17250_fp_kind, &
       300.78650_fp_kind,  303.37900_fp_kind,  305.95950_fp_kind,  308.52100_fp_kind,  310.91600_fp_kind, &
       313.64650_fp_kind,  315.24400_fp_kind,  315.24400_fp_kind,  315.24400_fp_kind,  315.24400_fp_kind /), &
    (/ N_LAYERS, N_PROFILES /) )

  REAL( fp_kind ), DIMENSION( N_LAYERS, N_PROFILES ) :: LAYER_WATER_VAPOR = RESHAPE( &
    (/   0.00101_fp_kind,    0.00122_fp_kind,    0.00162_fp_kind,    0.00146_fp_kind,    0.00256_fp_kind, &
         0.00396_fp_kind,    0.00396_fp_kind,    0.00404_fp_kind,    0.00389_fp_kind,    0.00359_fp_kind, &
         0.00306_fp_kind,    0.00286_fp_kind,    0.00292_fp_kind,    0.00256_fp_kind,    0.00239_fp_kind, &
         0.00260_fp_kind,    0.00257_fp_kind,    0.00237_fp_kind,    0.00235_fp_kind,    0.00233_fp_kind, &
         0.00231_fp_kind,    0.00230_fp_kind,    0.00229_fp_kind,    0.00228_fp_kind,    0.00228_fp_kind, &
         0.00229_fp_kind,    0.00230_fp_kind,    0.00231_fp_kind,    0.00232_fp_kind,    0.00233_fp_kind, &
         0.00234_fp_kind,    0.00234_fp_kind,    0.00233_fp_kind,    0.00234_fp_kind,    0.00236_fp_kind, &
         0.00239_fp_kind,    0.00242_fp_kind,    0.00246_fp_kind,    0.00249_fp_kind,    0.00252_fp_kind, &
         0.00257_fp_kind,    0.00267_fp_kind,    0.00279_fp_kind,    0.00294_fp_kind,    0.00314_fp_kind, &
         0.00333_fp_kind,    0.00353_fp_kind,    0.00376_fp_kind,    0.00417_fp_kind,    0.00472_fp_kind, &
         0.00535_fp_kind,    0.00610_fp_kind,    0.00685_fp_kind,    0.00752_fp_kind,    0.00815_fp_kind, &
         0.00838_fp_kind,    0.00821_fp_kind,    0.00857_fp_kind,    0.00967_fp_kind,    0.01246_fp_kind, &
         0.01880_fp_kind,    0.02720_fp_kind,    0.03583_fp_kind,    0.04462_fp_kind,    0.04548_fp_kind, &
         0.03811_fp_kind,    0.03697_fp_kind,    0.04440_fp_kind,    0.21296_fp_kind,    0.63316_fp_kind, &
         0.99454_fp_kind,    1.07315_fp_kind,    1.19554_fp_kind,    1.67418_fp_kind,    2.32281_fp_kind, &
         2.94968_fp_kind,    3.55697_fp_kind,    4.14798_fp_kind,    4.66578_fp_kind,    5.09204_fp_kind, &
         5.48664_fp_kind,    5.85228_fp_kind,    6.13708_fp_kind,    6.29674_fp_kind,    6.33826_fp_kind, &
         6.23438_fp_kind,    5.90642_fp_kind,    5.47632_fp_kind,    5.17642_fp_kind,    4.99432_fp_kind, &
         4.88358_fp_kind,    4.83193_fp_kind,    4.79100_fp_kind,    4.76039_fp_kind,    4.73565_fp_kind, &
         6.36824_fp_kind,    7.89683_fp_kind,    7.67263_fp_kind,    7.45786_fp_kind,    7.25201_fp_kind /), &
    (/ N_LAYERS, N_PROFILES /) )

  REAL( fp_kind ), DIMENSION( N_LAYERS, N_PROFILES ) :: LAYER_OZONE = RESHAPE( &
    (/   0.01180_fp_kind,    0.07742_fp_kind,    0.22580_fp_kind,    0.51383_fp_kind,    0.92374_fp_kind, &
         1.34979_fp_kind,    1.77667_fp_kind,    2.22106_fp_kind,    2.74227_fp_kind,    3.38562_fp_kind, &
         4.16397_fp_kind,    5.15877_fp_kind,    6.35705_fp_kind,    7.42968_fp_kind,    8.17368_fp_kind, &
         8.65675_fp_kind,    8.92960_fp_kind,    9.05599_fp_kind,    9.07657_fp_kind,    8.98850_fp_kind, &
         8.77807_fp_kind,    8.47996_fp_kind,    8.12254_fp_kind,    7.69423_fp_kind,    7.20706_fp_kind, &
         6.65447_fp_kind,    6.05961_fp_kind,    5.46423_fp_kind,    4.87438_fp_kind,    4.29937_fp_kind, &
         3.73859_fp_kind,    3.20185_fp_kind,    2.68818_fp_kind,    2.19109_fp_kind,    1.70980_fp_kind, &
         1.26143_fp_kind,    0.88349_fp_kind,    0.55506_fp_kind,    0.32425_fp_kind,    0.19747_fp_kind, &
         0.10714_fp_kind,    0.07026_fp_kind,    0.06153_fp_kind,    0.05869_fp_kind,    0.06146_fp_kind, &
         0.06426_fp_kind,    0.06714_fp_kind,    0.06989_fp_kind,    0.07170_fp_kind,    0.07272_fp_kind, &
         0.07346_fp_kind,    0.07383_fp_kind,    0.07406_fp_kind,    0.07418_fp_kind,    0.07424_fp_kind, &
         0.07411_fp_kind,    0.07379_fp_kind,    0.07346_fp_kind,    0.07312_fp_kind,    0.07284_fp_kind, &
         0.07274_fp_kind,    0.07273_fp_kind,    0.07272_fp_kind,    0.07270_fp_kind,    0.07257_fp_kind, &
         0.07233_fp_kind,    0.07167_fp_kind,    0.07047_fp_kind,    0.06920_fp_kind,    0.06803_fp_kind, &
         0.06729_fp_kind,    0.06729_fp_kind,    0.06753_fp_kind,    0.06756_fp_kind,    0.06717_fp_kind, &
         0.06615_fp_kind,    0.06510_fp_kind,    0.06452_fp_kind,    0.06440_fp_kind,    0.06463_fp_kind, &
         0.06484_fp_kind,    0.06487_fp_kind,    0.06461_fp_kind,    0.06417_fp_kind,    0.06382_fp_kind, &
         0.06378_fp_kind,    0.06417_fp_kind,    0.06482_fp_kind,    0.06559_fp_kind,    0.06638_fp_kind, &
         0.06722_fp_kind,    0.06841_fp_kind,    0.06944_fp_kind,    0.06720_fp_kind,    0.06046_fp_kind, &
         0.04124_fp_kind,    0.02624_fp_kind,    0.02623_fp_kind,    0.02622_fp_kind,    0.02622_fp_kind /), &
    (/ N_LAYERS, N_PROFILES /) )
