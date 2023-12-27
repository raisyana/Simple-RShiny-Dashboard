# R-SHINY PROGRAM (SERVER.UI)
server <- function(input, output){
    
    output$noteWeibull <- renderText( 
        "Parameter scale yang digunakan dalam pengujian distribusi weibull adalah Data Temperature Maksimal Provinsi Jawa Timur Tahun 2020
        dimana batas bawah dan batas atas data yang digunakan sebagai sample ditentukan oleh user.
        Sedangkan parameter shape merupakan sequence yang nilai minimal dan maksimalnya juga ditentuka oleh use." 
    )
    output$penjelasanWeibull <- renderText( 
        "Distribusi Weibull adalah distribusi probabilitas kontinu yang banyak digunakan untuk memodelkan dan menganalisa data waktu kegagalan. 
        Pada perhitungan distribusi weibull memiliki parameter skala dan shape." 
    )
    output$histWeibull <- renderPlot({
        x <- temp[input$weibull_batasbawah:input$weibull_batasatas]
        y <- seq(input$weibull_min, input$weibull_maks)
        weibull <- dweibull(x, shape = x, scale = y)
        histWeibull<- hist(weibull,
                           main = "DISTRIBUSI WEIBULL",
                           xlab = "Data Temperature Jawa Timur",
                           col = brewer.pal(5, "Pastel2"),
                           border = "white")
    })
    output$weibull_hasil <- renderDataTable({
        x <- temp[input$weibull_batasbawah:input$weibull_batasatas]
        y <- seq(input$weibull_min, input$weibull_maks)
        weibull <- dweibull(x, shape = x, scale = y)
        weibull_tabel <- data.frame(seq(1, length(x)), weibull)
        colnames(weibull_tabel) <- c("SEQUENCE","PROBABILITAS HASIL WEIBULL")
        expr = weibull_tabel
        
    })
    
# ====================DISTRIBUSI EKSPONENSIAL [R - EXP]=====================================================================================================    
    output$noterExp <- renderText( # BELUM NEMU YG BAGUS
        "Dalam pengujian random sampling distribusi ekponensial, sample data diambil dari Data Temperature Maksimal Provinsi Jawa Timur Tahun 2020
        dimana ibatas bawah dan batas atas untuk sample data ditentukan oleh user. Parameter lain yang digunakan adalah rate atau lambda yang diset
        dengan nilai 10, namun nilai ini dapat diubah sesuai keinginan user." 
    )
    output$penjelasanrExp <- renderText( # KURANG KATA - KATANYA
        "Fungsi random sampling distribusi eksponensial merupakan fungsi yang digunakan untuk mensimulasikan sekumpulan bilangan acak yang diambi
        dari distribusi eksponensial." 
    )
    output$histrExp <- renderPlot({
        rExp_sample <- temp[input$rExp_batasbawah:input$rExp_batasatas]
        rEksponensial <- rexp(rExp_sample, input$rExp_lambda)
        histEksponensial<- hist(rEksponensial,
                                main = "RANDOM SAMPLING FROM EXPONENTIAL DISTRIBUTION",
                                xlab = "Data Temperature Jawa Timur",
                                col = brewer.pal(9, "Pastel1"),
                                border = "white")
    })
    output$rExp_hasil <- renderDataTable({
        rExp_sample <- temp[input$rExp_batasbawah:input$rExp_batasatas]
        rEksponensial <- rexp(rExp_sample, input$rExp_lambda)
        rEks_tabel <- data.frame(rExp_sample, rEksponensial)
        colnames(rEks_tabel) <- c("TEMPERATURE SAMPLE","PROBABILITAS HASIL R - EXP")
        expr = rEks_tabel
        
    })
    
# ====================DISTRIBUSI EKSPONENSIAL [Q - EXP]=====================================================================================================    
    output$noteqExp <- renderText( 
        "Nilai value berupa quantile yang digunakan berasal dari hasil perhitungan peluang data.
        Paramater lower.tail yang bernilai TRUE digunakan agar pengujian yang dilakukan akan mengembalikan nilai dari sisi sebalah kiri nilai value dan parameter
        log.p yang diset dengan nilai FALSE." 
    )
    output$penjelasanqExp <- renderText( 
        "Quantile function dari distribusi eksponensial merupakan fungsi yang akan menghasilkan nilai kemungkinan atau peluang yang sesuai dengan
        nilai quantile yang telah ditentukan." 
    )
    output$histqExp <- renderPlot({
        prob.range <- dPro$Freq
        temp.qEks <- qexp(prob.range, lower.tail = TRUE, log.p = FALSE)
        temp.df <- data.frame("Temperature" = prob.range, "Density" = temp.qEks)
        histqEksponensial<- hist(temp.qEks,
                                 main = "QUANTILE FUNCTION OF EXPONENTIAL DISTRIBUTION",
                                 xlab = "Data Temperature Jawa Timur",
                                 breaks = input$qExp_slider,
                                 col = brewer.pal(3, "Pastel1"),
                                 border = "white")
    })
    output$qExp_hasil <- renderDataTable({
        prob.range <- dPro$Freq
        temp.qEks <- qexp(prob.range, lower.tail = TRUE, log.p = FALSE)
        qEks_tabel <- data.frame(seq(1,60), temp.qEks)
        colnames(qEks_tabel) <- c("SEQUENCE","PROBABILITAS HASIL Q - EXP")
        expr = qEks_tabel
        
    })
    
# ====================DISTRIBUSI EKSPONENSIAL [P - EXP]=====================================================================================================    
    output$notepExp <- renderText( 
        "Dataset yang digunakan adalah Data Temperature Maksimal Provinsi Jawa Timur Tahun 2020 dengan sample data yang diuji sesuai dengan input user.
        Dengan tambahan parameter lower.tail yang bernilai TRUE, artinya nilai yang dikembalikan berada di sebelah kiri nilai value. Dan parameter 
        log.p yang menyimpan nilai FALSE." 
    )
    output$penjelasanpExp <- renderText( 
        "Cumulative Density Function dari distribusi eksponensial merupakan fungsi yang akan mengambalikan nilai yang sesuai
        dari fungsi distribusi kumulatif eksponensial untuk vektor quantile yang telah ditentukan." 
    )
    output$plotpExp <- renderPlotly({
        sample.range <- temp[input$pExp_batasbawah:input$pExp_batasatas]
        temp.pEks <- pexp(sample.range, lower.tail = TRUE, log.p = FALSE)
        temp.df <- data.frame("Temperature" = sample.range, "Density" = temp.pEks)
        plotpEks <- ggplot(temp.df, aes(x = Temperature, y = Density)) + 
            geom_point(col = "skyblue") +
            labs(title = "CUMULATIVE DENSITY FUNCTION OF EXPONENTIAL DISTRIBUTION", x = "RANDOM TEMPERATURE", y = "DENSITY")+
            theme_minimal()
        ggplotly(plotpEks)
    })    
    output$histpExp <- renderPlot({
        sample.range <- temp[input$pExp_batasbawah:input$pExp_batasatas]
        temp.pEks <- pexp(sample.range, lower.tail = TRUE, log.p = FALSE)
        plotdEksponensial <- plot(sample.range, temp.pEks, type = "h", xlab = "RANDOM TEMPERATURE", ylab = "DENSITY", 
                                  main = "CUMULATIVE DENSITY FUNCTION OF EKSPONENTIAL DISTRIBUTION")
    })
    output$pExp_hasil <- renderDataTable({
        sample.range <- temp[input$pExp_batasbawah:input$pExp_batasatas]
        temp.pEks <- pexp(sample.range, lower.tail = TRUE, log.p = FALSE)
        pEks_tabel <- data.frame(sample.range, temp.pEks)
        colnames(pEks_tabel) <- c("TEMPERATURE SAMPLE","PROBABILITAS HASIL P - EXP")
        expr = pEks_tabel
        
    })
    
# ====================DISTRIBUSI EKSPONENSIAL [D - EXP]=====================================================================================================    
    output$notedExp <- renderText( 
        "Parameter yang dibutuhkan untuk uji densitas distribusi eksponensial adalah nilai value berupa sample temperature dari Data Temperature Maksimal 
        Provinsi Jawa Timur Tahun 2020, nilai rate, dan log. User dapat menentukan sample data dan nilai rate yang ingin diuji. Namun, nilai log akan 
        diset dengan nilai FALSE." 
    )
    output$penjelasandExp <- renderText( 
        "Fungsi density distribusi eksponensial merupakan fungsi yang akan mengembalikan nilai yang sesuai dari kerapatan eksponensial 
        untuk vektor dari quantile yang telah ditentukan." 
    )
    output$plotdExp <- renderPlotly({
        sample.range <- temp[input$dExp_batasbawah:input$dExp_batasatas]
        temp.dEks <- dexp(sample.range, rate = input$dExp_rate, log = FALSE)
        temp.df <- data.frame("Temperature" = sample.range, "Density" = temp.dEks)
        plotdEks <- ggplot(temp.df, aes(x = Temperature, y = Density)) + 
            geom_point(col = "navy") +
            labs(title = "DENSITY FUNCTION OF EXPONENTIAL DISTRIBUTION", x = "TEMPERATURE", y = "DENSITY")+
            theme_minimal()
        ggplotly(plotdEks)
    })
    output$dExp_hasil <- renderDataTable({
        sample.range <- temp[input$dExp_batasbawah:input$dExp_batasatas]
        temp.dEks <- dexp(sample.range, rate = input$dExp_rate, log = FALSE)
        dEks_tabel <- data.frame(sample.range, temp.dEks)
        colnames(dEks_tabel) <- c("TEMPERATURE SAMPLE","PROBABILITAS HASIL D - EXP")
        expr = dEks_tabel
        
    })
    
# ====================DISTRIBUSI NORMAL [R - NORM]===========================================================================================================    
    output$noterNormal <- renderText( 
        "Parameter uji yang digunakan adalah nilai value, mean, dan simpangan baku dari Data Temperature Maksimal Provinsi Jawa Timur Tahun 2020.
        Hasil dari uji random sampling akan berupa vektor dari 353 variabel acak berdistribusi normal dengan nilai rata - rata dan simpangan sesuai
        input dari user." 
    )
    output$penjelasanrNorm <- renderText( 
        "Fungsi random sampling digunakan untuk menggambarkan sample acak dari distribusi normal. Fungsi ini akan menghasilkan vektor acak berdistribusi normal 
        dengan panjang vektor n, mean populasi, dan simpangan baku populasi." 
    )
    output$histrNormal <- renderPlot({
        rNorm_mean <- input$rNorm_mean
        rNorm_sd <- input$rNorm_sd
        rNormal <- rnorm(353, rNorm_mean, rNorm_sd^0.5)
        histrNormal<- hist(rNormal,
                           main = "RANDOM SAMPLING FROM NORMAL DISTRIBUTION",
                           xlab = "Data Temperature Jawa Timur",
                           col = brewer.pal(9, "PuBuGn"),
                           border = "white")
    })
    output$rNorm_hasil <- renderDataTable({
        rNorm_mean <- input$rNorm_mean
        rNorm_sd <- input$rNorm_sd
        rNormal <- rnorm(353, rNorm_mean, rNorm_sd^0.5)
        rNorm_tabel <- data.frame(seq(1,353), rNormal)
        colnames(rNorm_tabel) <- c("VEKTOR","PROBABILITAS HASIL R - NORM")
        expr = rNorm_tabel
        
    })
    
# ====================DISTRIBUSI NORMAL [Q - NORM]===========================================================================================================    
    output$noteqNormal <- renderText( 
        "Pada uji quantile distribusi normal, nilai value yang digunakan diambil dari hasil perhitungan peluang berdasarkan 
        Data Temperature Maksimal Provinsi Jawa Timur Tahun 2020. Dengan parameter lain berupa nilai rata - rata dan simpangan 
        baku yang dapat diatur oleh keinginan user." 
    )
    output$penjelasanqNorm <- renderText(
        "Fungsi quantile pada distribusi normal merupakan kebalikan atau invers dari fungsi kepadatan kumulatif (CDF). Sehingga, 
        hasil dari uji quantile ini akan memetakan dari probabilitas ke nilai value." 
    )
    output$plotqNormal <- renderPlotly({
        prob.range <- dPro$Freq
        qNorm_mean <- input$qNorm_mean
        qNorm_sd <- input$qNorm_sd
        icdf.df <- data.frame("Probability" = prob.range, "Temperature" = qnorm(prob.range, qNorm_mean, qNorm_sd))
        plotqNorm <- ggplot(icdf.df, aes(x = Probability, y = Temperature)) + 
            geom_point(col = "orange") +
            labs(title = "QUANTILE FUNCTION OF NORMAL DISTRIBUTION", x = "PROBABILITY", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(plotqNorm)
    })
    output$histqNormal <- renderPlot({
        qNorm_mean <- input$qNorm_mean
        qNorm_sd <- input$qNorm_sd
        x <- table(temp)/length(temp)
        dPro <- data.frame(x)
        prob.range <- dPro$Freq
        icdf.df <- qnorm(prob.range, qNorm_mean, qNorm_sd)
        histqNormal<- hist(icdf.df,
                           main = "QUANTILE FUNCTION OF NORMAL DISTRIBUTION",
                           xlab = "Data Temperature Jawa Timur",
                           col = brewer.pal(7, "PuBuGn"),
                           border = "white")
    })
    output$qNorm_hasil <- renderDataTable({
        prob.range <- dPro$Freq
        qNorm_mean <- input$qNorm_mean
        qNorm_sd <- input$qNorm_sd
        hitung <- qnorm(prob.range, qNorm_mean, qNorm_sd)
        qNorm_tabel <- data.frame(prob.range, hitung)
        colnames(qNorm_tabel) <- c("TEMPERATURE SAMPLE","PROBABILITAS HASIL Q - NORM")
        expr = qNorm_tabel
        
    })
    
# ====================DISTRIBUSI NORMAL [P - NORM]===========================================================================================================    
    output$notepNormal <- renderText( 
        "Pada pengujian cumulative density function sample data yang digunakan diambil dari Data Temperature 
        Maksimal Provinsi Jawa Timur Tahun 2020, dimana batas bawah dan batas atas data sample dapat ditentukan oleh user begitu juga 
        dengan parameter lain seperti, nilai rata - rata dan simpangan baku. Namun pada setelan awal sample data akan diambil dari dataset
        urutan ke 50 hingga 250 dengan nilai rata - rata = 29 dan simpangan baku = 1,2 dan ditambah dengan parameter lower.tail yang diset
        sesuai default yaitu TRUE. Nantinya plot yang dihasilkan akan bergerak mendekati nilai nol." 
    )
    output$penjelasanpNorm <- renderText( 
        "Cumulative Density Function (CDF) adalah fungsi kepadatan kumulatif dari distribusi normal suatu variabel acak tertentu.
        Hasil uji kumulatif akan sama dengan yang diperoleh dari menjumlahkan secara manual probabilitas yang diperoleh
        melalu uji densitas distribusi normal. Terdapat penambahan argumen lower.tail yang digunakan sebagai parameter di uji kumulatif, 
        lower.tail sendiri digunakan untuk mengatur area mana yang ingin dilakukan uji kumulatif. Secara default lower.tail akan bernilai 
        TRUE yang artinya akan mengembalikan area sebelah kiri nilai value yang diberikan dalam distribusi normal." 
    )
    output$plotpNormal <- renderPlotly({
        sample.range <- temp[input$pNorm_batasbawah:input$pNorm_batasatas]
        temp.mean <- input$pNorm_mean
        temp.sd <- input$pNorm_sd
        cdf <- pnorm(sample.range, temp.mean, temp.sd, lower.tail = TRUE)
        temp.df <- data.frame("Temperature" = sample.range, "Density" = temp.dNorm)
        temp.df <- cbind(temp.df, "CDF_LowerTail" = cdf)
        plotpNorm <- ggplot(temp.df, aes(x = Temperature, y = CDF_LowerTail)) + 
            geom_point(col = "red") +
            labs(title = "CUMULATIVE DENSITY FUNCTION OF NORMAL DISTRIBUTION", x = "RANDOM TEMPERATURE", y = "DENSITY")+
            theme_minimal()
        ggplotly(plotpNorm)
    })
    output$histpNormal <- renderPlot({
        pNorm.sample <- temp[input$pNorm_batasbawah:input$pNorm_batasatas]
        temp.mean <- input$pNorm_mean 
        temp.sd <- input$pNorm_sd
        cdf <- pnorm(pNorm.sample, temp.mean, temp.sd, lower.tail = TRUE)
        histpNorm<- hist(cdf,
                         main = "CUMULATIVE DENSITY FUNCTION OF NORMAL DISTRIBUTION",
                         xlab = "Data Temperature Jawa Timur",
                         col = brewer.pal(5, "PuBuGn"),
                         border = "white")
    })
    output$pNorm_hasil <- renderDataTable({
        sample.range <- temp[input$pNorm_batasbawah:input$pNorm_batasatas]
        temp.mean <- input$pNorm_mean
        temp.sd <- input$pNorm_sd
        cdf <- pnorm(sample.range, temp.mean, temp.sd, lower.tail = TRUE)
        pNorm_tabel <- data.frame(sample.range, cdf)
        colnames(pNorm_tabel) <- c("TEMPERATURE SAMPLE","PROBABILITAS HASIL P - NORM")
        expr = pNorm_tabel
        
    })
    
# ====================DISTRIBUSI NORMAL [D - NORM]===========================================================================================================    
    output$notedNormal <- renderText( 
        "Pada pengujian densitas distribusi normal, digunakan sample Data Temperature Maksimal Provinsi Jawa Timur Tahun 2020.
        Dimana batas bawah dan batas atas data yang akan digunakan sebagai sample ditentukan berdasarkan input user, begitu juga
        dengan nilai rata - rata dan simpangan baku yang diperlukan sebagai parameter pegujian." 
    )
    output$penjelasandNorm <- renderText( # KURANG KATA - KATANYA
        "Probability Density Function atau distribusi densitas menunjukkan nilai probabilitas
        dari sebuah data yang diamati dengan pengukuran nilai tertentu. Pada uji densitas normal
        digunakan parameter x sebagai value, miu yang mendefinisikan mean, dan sigma menyimpan 
        nilai simpangan baku." 
    )
    output$plotdNormal <- renderPlotly({
        dNorm.sample <- temp[input$dNorm_batasbawah:input$dNorm_batasatas]
        temp.mean <- input$dNorm_mean
        temp.sd <- input$dNorm_sd
        temp.dNorm <- dnorm(dNorm.sample, mean = temp.mean, sd = temp.sd)
        temp.df <- data.frame("Temperature" = dNorm.sample, "Density" = temp.dNorm)
        plotdNorm <- ggplot(temp.df, aes(x = Temperature, y = Density)) + 
            geom_point(col = "maroon") +
            labs(title = "DENSITY FUNCTION OF NORMAL DISTRIBUTION", x = "TEMPERATURE", y = "DENSITY")+
            theme_minimal()
        ggplotly(plotdNorm)
    })
    output$histdNormal <- renderPlot({
        dNorm.sample <- temp[input$dNorm_batasbawah:input$dNorm_batasatas]
        temp.mean <- input$dNorm_mean
        temp.sd <- input$dNorm_sd
        temp.dNorm <- dnorm(dNorm.sample, mean = temp.mean, sd = temp.sd)
        histdNorm <- hist(temp.dNorm,
                          main = "DENSITY FUNCTION OF NORMAL DISTRIBUTION",
                          xlab = "Data Temperature Jawa Timur",
                          col = brewer.pal(3, "PuBuGn"),
                          border = "white")
    })
    output$dNorm_hasil <- renderDataTable({
        dNorm.sample <- temp[input$dNorm_batasbawah:input$dNorm_batasatas]
        temp.mean <- input$dNorm_mean
        temp.sd <- input$dNorm_sd
        temp.dNorm <- dnorm(dNorm.sample, mean = temp.mean, sd = temp.sd)
        dNorm_tabel <- data.frame(dNorm.sample, temp.dNorm)
        colnames(dNorm_tabel) <- c("TEMPERATURE SAMPLE","PROBABILITAS HASIL D - NORM")
        expr = dNorm_tabel
        
    })
    
# ====================DISTRIBUSI UNIFORM====================================================================================================================    
    output$noteUniform <- renderText( 
        "Dalam pengujian distribusi uniform ini data yang dibangkitkan sesuai dengan dataset, yaitu Data Temperature Maksimal Provinsi Jawa Timur Tahun 2020.
        Data tersebut akan dicetak ulang sebagai sample pengujian. Banyaknya sample dapat disesuaikan dengan input dari user, namun sample awal akan diset sebanyak
        1000 kali dengan perulangan. " 
    )
    output$penjelasanUniform <- renderText( 
        "Distribusi peluang uniform adalah distribusi yang mempunyai probabilitas yang sama pada setiap kejadian, tidak dikategorikan, 
        dan ruang sampelnya tidak dibatasi." 
    )
    output$histUniform <- renderPlot({
        generateRandom <- sample(temp, size = input$unif_input, replace = TRUE)
        generateRandom <- generateRandom/input$unif_input
        minimal <- min(generateRandom)
        maksimal <- max(generateRandom)
        histUniform<- hist(generateRandom,
                           main = "PEMBANGKITAN DISTRIBUSI KONTINYU UNIFORM",
                           xlab = "Variable Random Dari Data Temperature Jawa Timur",
                           xlim = c(minimal, maksimal),
                           col = brewer.pal(3, "Pastel2"),
                           border = "white")
    })
    output$unif_hasil <- renderDataTable({
        generateRandom <- sample(temp, size = input$unif_input, replace = TRUE)
        hitung <- generateRandom/input$unif_input
        unif_tabel <- data.frame(generateRandom, hitung)
        colnames(unif_tabel) <- c("TEMPERATURE SAMPLE","PROBABILITAS HASIL DISTRIBUSI UNIFORM")
        expr=unif_tabel
    })
    
# ====================HOME===================================================================================================================================    
    output$temperature <- renderDataTable(dataku)
    
    output$plotDesember<- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = DESEMBER)) +
            geom_line(col = "navy") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN DESEMBER", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotNovember <- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = NOVEMBER)) +
            geom_line(col = "navy") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN NOVEMBER", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotOktober<- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = OKTOBER)) +
            geom_line(col = "skyblue") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN OKTOBER", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotSeptember <- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = SEPTEMBER)) +
            geom_line(col = "navy") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM SEPTEMBER", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotAgustus<- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = AGUSTUS)) +
            geom_line(col = "skyblue") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN AGUSTUS", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotJuli <- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = JULI)) +
            geom_line(col = "navy") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN JULI", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotJuni<- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = JUNI)) +
            geom_line(col = "skyblue") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN JUNI", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotMei <- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = MEI)) +
            geom_line(col = "skyblue") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN MEI", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotApril<- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = APRIL)) +
            geom_line(col = "navy") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN APRIL", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotMaret <- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = MARET)) +
            geom_line(col = "skyblue") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN MARET", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotFebruari<- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = FEBRUARI)) +
            geom_line(col = "navy") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN FEBRUARI", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
    output$plotJanuari <- renderPlotly({
        diagram <- ggplot(data = dataku, aes(x = TANGGAL, y = JANUARI)) +
            geom_line(col = "skyblue") +
            geom_point(col = "pink") +
            labs(title = "TEMPERATURE MAKSIMUM BULAN JANUARI", x = "TANGGAL", y = "TEMPERATURE")+
            theme_minimal()
        ggplotly(diagram)
    })
    
} # tutup kurung