library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(ggplot2)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(readr)
library(car) 
library(rmarkdown)
library(knitr)
library(webshot)
library(lmtest)

data_variabel <- read_csv("sovi_data.csv")
data_matriks <- read_csv("distance.csv")
variables <- names(data_variabel)

map_file <- "sovi_data.geojson"
map_data <- st_read(map_file, quiet = TRUE)

data_variabel$DISTRICTCODE <- as.character(data_variabel$DISTRICTCODE)
data_lengkap <- left_join(
  data_variabel,                          
  st_drop_geometry(map_data[c("kodeprkab", "nmkab")]), 
  by = c("DISTRICTCODE" = "kodeprkab")         
)

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard AKSI"),
  
  dashboardSidebar(
    width = 250,
    div(style = "padding: 10px; text-align: center; font-weight: bold; font-size: 16px;", "MENU"),
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("search"),
               startExpanded = FALSE,
               menuSubItem("Statistik Deskriptif", tabName = "deskriptif", icon = icon("chart-line")),
               menuSubItem("Grafik", tabName = "grafik", icon = icon("chart-bar")),
               menuSubItem("Peta", tabName = "peta", icon = icon("globe")),
               menuSubItem("Tabel", tabName = "tabel", icon = icon("table"))
      ),
      menuItem("Uji Asumsi", tabName = "uji", icon = icon("calculator")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("chart-area"),
               startExpanded = FALSE,
               menuSubItem("Uji Beda Rata-Rata",
                           tabName = "rata",
                           icon = icon("balance-scale")),
               menuSubItem("Uji Proporsi",
                           tabName = "proporsi",
                           icon = icon("percentage")),
               menuSubItem("Uji Variance",
                           tabName = "variance",
                           icon = icon("expand-arrows-alt")),
               menuSubItem("Uji ANOVA",
                           tabName = "anova",
                           icon = icon("layer-group"))
      ),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    tags$head(
      # Import Google Fonts
      tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap", rel = "stylesheet"),
      
      tags$style(HTML("
        /* Import Google Fonts */
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap');
        
        /* Root Variables for Consistent Color Scheme */
        :root {
          --primary-color: #2563eb;
          --primary-dark: #1d4ed8;
          --primary-light: #3b82f6;
          --secondary-color: #64748b;
          --secondary-dark: #475569;
          --secondary-light: #94a3b8;
          --accent-color: #06b6d4;
          --accent-dark: #0891b2;
          --success-color: #10b981;
          --warning-color: #f59e0b;
          --danger-color: #ef4444;
          --info-color: #3b82f6;
          --dark-color: #1e293b;
          --light-color: #f8fafc;
          --white: #ffffff;
          --gray-50: #f9fafb;
          --gray-100: #f3f4f6;
          --gray-200: #e5e7eb;
          --gray-300: #d1d5db;
          --gray-400: #9ca3af;
          --gray-500: #6b7280;
          --gray-600: #4b5563;
          --gray-700: #374151;
          --gray-800: #1f2937;
          --gray-900: #111827;
        }
        
        /* Global Font Settings */
        * {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        }
        
        body {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          font-size: 14px;
          line-height: 1.6;
          color: var(--gray-700);
          background-color: var(--gray-50);
        }
        
        /* Header Styling */
        .main-header {
          background: linear-gradient(135deg, var(--primary-color) 0%, var(--primary-dark) 100%) !important;
          border-bottom: 3px solid var(--primary-dark) !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06) !important;
        }
        
        .main-header .navbar-brand {
          font-weight: 700 !important;
          font-size: 20px !important;
          color: var(--white) !important;
          text-shadow: 0 1px 2px rgba(0, 0, 0, 0.1) !important;
        }
        
        /* Sidebar Styling */
        .main-sidebar {
          background: linear-gradient(180deg, var(--dark-color) 0%, var(--gray-800) 100%) !important;
          box-shadow: 4px 0 6px -1px rgba(0, 0, 0, 0.1) !important;
        }
        
        .sidebar-menu > li > a {
          color: var(--gray-300) !important;
          font-weight: 500 !important;
          font-size: 14px !important;
          padding: 12px 15px !important;
          transition: all 0.3s ease !important;
          border-left: 3px solid transparent !important;
        }
        
        .sidebar-menu > li > a:hover {
          background: linear-gradient(90deg, rgba(37, 99, 235, 0.1) 0%, rgba(37, 99, 235, 0.05) 100%) !important;
          color: var(--primary-light) !important;
          border-left: 3px solid var(--primary-color) !important;
          transform: translateX(2px) !important;
        }
        
        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, var(--primary-color) 0%, var(--primary-dark) 100%) !important;
          color: var(--white) !important;
          border-left: 3px solid var(--accent-color) !important;
          font-weight: 600 !important;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1) !important;
        }
        
        .sidebar-menu .treeview-menu > li > a {
          color: var(--gray-400) !important;
          font-size: 13px !important;
          padding-left: 35px !important;
          font-weight: 400 !important;
        }
        
        .sidebar-menu .treeview-menu > li > a:hover {
          color: var(--primary-light) !important;
          background-color: rgba(37, 99, 235, 0.08) !important;
        }
        
        .sidebar-menu .treeview-menu > li.active > a {
          color: var(--accent-color) !important;
          background-color: rgba(6, 182, 212, 0.1) !important;
          font-weight: 500 !important;
        }
        
        /* Content Area */
        .content-wrapper {
          background-color: var(--gray-50) !important;
          min-height: 100vh !important;
        }
        
        .content {
          padding: 20px !important;
        }
        
        /* Box Styling */
        .box {
          border-radius: 12px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06) !important;
          border: 1px solid var(--gray-200) !important;
          background-color: var(--white) !important;
          margin-bottom: 20px !important;
          transition: all 0.3s ease !important;
        }
        
        .box:hover {
          box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05) !important;
          transform: translateY(-1px) !important;
        }
        
        .box-header {
          background: linear-gradient(135deg, var(--white) 0%, var(--gray-50) 100%) !important;
          border-bottom: 2px solid var(--gray-100) !important;
          border-radius: 12px 12px 0 0 !important;
          padding: 15px 20px !important;
        }
        
        .box-header.with-border {
          border-bottom: 2px solid var(--gray-100) !important;
        }
        
        .box-title {
          font-size: 18px !important;
          font-weight: 600 !important;
          color: var(--gray-800) !important;
          margin: 0 !important;
        }
        
        .box-body {
          padding: 20px !important;
          background-color: var(--white) !important;
        }
        
        /* Box Colors */
        .box-primary .box-header {
          background: linear-gradient(135deg, var(--primary-color) 0%, var(--primary-dark) 100%) !important;
          color: var(--white) !important;
        }
        
        .box-primary .box-title {
          color: var(--white) !important;
        }
        
        .box-success .box-header {
          background: linear-gradient(135deg, var(--success-color) 0%, #059669 100%) !important;
          color: var(--white) !important;
        }
        
        .box-success .box-title {
          color: var(--white) !important;
        }
        
        .box-info .box-header {
          background: linear-gradient(135deg, var(--info-color) 0%, var(--primary-dark) 100%) !important;
          color: var(--white) !important;
        }
        
        .box-info .box-title {
          color: var(--white) !important;
        }
        
        .box-warning .box-header {
          background: linear-gradient(135deg, var(--warning-color) 0%, #d97706 100%) !important;
          color: var(--white) !important;
        }
        
        .box-warning .box-title {
          color: var(--white) !important;
        }
        
        /* Value Boxes */
        .small-box {
          border-radius: 12px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06) !important;
          transition: all 0.3s ease !important;
          border: none !important;
        }
        
        .small-box:hover {
          transform: translateY(-2px) !important;
          box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05) !important;
        }
        
        .small-box h3 {
          font-size: 28px !important;
          font-weight: 700 !important;
          font-family: 'JetBrains Mono', monospace !important;
        }
        
        .small-box p {
          font-size: 14px !important;
          font-weight: 500 !important;
        }
        
        /* Buttons */
        .btn {
          border-radius: 8px !important;
          font-weight: 500 !important;
          font-size: 14px !important;
          padding: 10px 20px !important;
          transition: all 0.3s ease !important;
          border: none !important;
          text-transform: none !important;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1) !important;
        }
        
        .btn:hover {
          transform: translateY(-1px) !important;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15) !important;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, var(--primary-color) 0%, var(--primary-dark) 100%) !important;
          color: var(--white) !important;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, var(--primary-dark) 0%, #1e40af 100%) !important;
          color: var(--white) !important;
        }
        
        .btn-success {
          background: linear-gradient(135deg, var(--success-color) 0%, #059669 100%) !important;
          color: var(--white) !important;
        }
        
        .btn-info {
          background: linear-gradient(135deg, var(--accent-color) 0%, var(--accent-dark) 100%) !important;
          color: var(--white) !important;
        }
        
        .btn-warning {
          background: linear-gradient(135deg, var(--warning-color) 0%, #d97706 100%) !important;
          color: var(--white) !important;
        }
        
        .btn-sm {
          padding: 6px 12px !important;
          font-size: 12px !important;
          border-radius: 6px !important;
        }
        
        /* Form Controls */
        .form-control {
          border: 2px solid var(--gray-200) !important;
          border-radius: 8px !important;
          padding: 10px 12px !important;
          font-size: 14px !important;
          transition: all 0.3s ease !important;
          background-color: var(--white) !important;
        }
        
        .form-control:focus {
          border-color: var(--primary-color) !important;
          box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.1) !important;
          outline: none !important;
        }
        
        .form-group label {
          font-weight: 500 !important;
          color: var(--gray-700) !important;
          margin-bottom: 6px !important;
          font-size: 14px !important;
        }
        
        /* Select2 Styling */
        .select2-container--default .select2-selection--single {
          border: 2px solid var(--gray-200) !important;
          border-radius: 8px !important;
          height: 42px !important;
          line-height: 38px !important;
        }
        
        .select2-container--default .select2-selection--single:focus {
          border-color: var(--primary-color) !important;
        }
        
        /* Tables */
        .table {
          background-color: var(--white) !important;
          border-radius: 8px !important;
          overflow: hidden !important;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1) !important;
        }
        
        .table thead th {
          background: linear-gradient(135deg, var(--gray-100) 0%, var(--gray-200) 100%) !important;
          color: var(--gray-800) !important;
          font-weight: 600 !important;
          font-size: 13px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
          border: none !important;
          padding: 15px 12px !important;
        }
        
        .table tbody td {
          padding: 12px !important;
          border-color: var(--gray-100) !important;
          font-size: 14px !important;
          color: var(--gray-700) !important;
        }
        
        .table tbody tr:hover {
          background-color: var(--gray-50) !important;
        }
        
        /* DataTables */
        .dataTables_wrapper {
          font-family: 'Inter', sans-serif !important;
        }
        
        .dataTables_filter input {
          border: 2px solid var(--gray-200) !important;
          border-radius: 8px !important;
          padding: 8px 12px !important;
          margin-left: 8px !important;
        }
        
        .dataTables_length select {
          border: 2px solid var(--gray-200) !important;
          border-radius: 8px !important;
          padding: 6px 10px !important;
        }
        
        .paginate_button {
          border-radius: 6px !important;
          margin: 0 2px !important;
        }
        
        .paginate_button.current {
          background: var(--primary-color) !important;
          color: var(--white) !important;
          border: 1px solid var(--primary-color) !important;
        }
        
        /* Custom Classes */
        .interpretation-box {
          margin-top: 20px !important;
          padding: 20px !important;
          background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%) !important;
          border-radius: 12px !important;
          border-left: 4px solid var(--primary-color) !important;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05) !important;
          font-size: 14px !important;
          line-height: 1.6 !important;
          color: var(--gray-700) !important;
        }
        
        .download-buttons {
          margin-top: 15px !important;
          padding: 20px !important;
          background: linear-gradient(135deg, var(--gray-50) 0%, var(--white) 100%) !important;
          border-radius: 12px !important;
          border: 2px solid var(--gray-100) !important;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05) !important;
        }
        
        .download-buttons h5 {
          color: var(--gray-800) !important;
          font-weight: 600 !important;
          margin-bottom: 15px !important;
          font-size: 16px !important;
        }
        
        .variable-selection {
          background: linear-gradient(135deg, var(--white) 0%, var(--gray-50) 100%) !important;
          padding: 20px !important;
          border-radius: 12px !important;
          border: 2px solid var(--gray-100) !important;
          margin-bottom: 15px !important;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05) !important;
        }
        
        .variable-selection h5 {
          color: var(--gray-800) !important;
          font-weight: 600 !important;
          margin-bottom: 15px !important;
          font-size: 16px !important;
        }
        
        .assumption-box {
          background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%) !important;
          padding: 20px !important;
          border-radius: 12px !important;
          border-left: 4px solid var(--warning-color) !important;
          margin-bottom: 15px !important;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05) !important;
        }
        
        .assumption-box h4 {
          color: #92400e !important;
          font-weight: 600 !important;
          margin-bottom: 10px !important;
        }
        
        .result-box {
          background: linear-gradient(135deg, #e0f2fe 0%, #b3e5fc 100%) !important;
          padding: 20px !important;
          border-radius: 12px !important;
          border-left: 4px solid var(--accent-color) !important;
          margin-bottom: 15px !important;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05) !important;
        }
        
        .result-box h5 {
          color: #0e7490 !important;
          font-weight: 600 !important;
          margin-bottom: 15px !important;
        }
        
        /* Tab Styling */
        .nav-tabs {
          border-bottom: 2px solid var(--gray-200) !important;
          margin-bottom: 20px !important;
        }
        
        .nav-tabs > li > a {
          border: none !important;
          border-radius: 8px 8px 0 0 !important;
          color: var(--gray-600) !important;
          font-weight: 500 !important;
          padding: 12px 20px !important;
          margin-right: 4px !important;
          transition: all 0.3s ease !important;
        }
        
        .nav-tabs > li > a:hover {
          background-color: var(--gray-100) !important;
          color: var(--gray-800) !important;
        }
        
        .nav-tabs > li.active > a {
          background: linear-gradient(135deg, var(--primary-color) 0%, var(--primary-dark) 100%) !important;
          color: var(--white) !important;
          border: none !important;
          font-weight: 600 !important;
        }
        
        /* Code/Verbatim Output */
        pre, code {
          font-family: 'JetBrains Mono', 'Fira Code', 'Consolas', monospace !important;
          font-size: 13px !important;
          line-height: 1.5 !important;
          background-color: var(--gray-100) !important;
          border: 1px solid var(--gray-200) !important;
          border-radius: 8px !important;
          padding: 15px !important;
          color: var(--gray-800) !important;
        }
        
        .shiny-text-output {
          font-family: 'JetBrains Mono', monospace !important;
          font-size: 13px !important;
          line-height: 1.6 !important;
          background-color: var(--gray-50) !important;
          border: 1px solid var(--gray-200) !important;
          border-radius: 8px !important;
          padding: 15px !important;
          color: var(--gray-800) !important;
          white-space: pre-wrap !important;
        }
        
        /* Leaflet Map */
        .leaflet-container {
          border-radius: 12px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1) !important;
        }
        
        /* Progress Bars */
        .progress {
          height: 8px !important;
          border-radius: 4px !important;
          background-color: var(--gray-200) !important;
        }
        
        .progress-bar {
          background: linear-gradient(90deg, var(--primary-color) 0%, var(--primary-light) 100%) !important;
          border-radius: 4px !important;
        }
        
        /* Alerts */
        .alert {
          border-radius: 8px !important;
          border: none !important;
          padding: 15px 20px !important;
          font-weight: 500 !important;
        }
        
        .alert-info {
          background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%) !important;
          color: #1e40af !important;
          border-left: 4px solid var(--primary-color) !important;
        }
        
        .alert-success {
          background: linear-gradient(135deg, #f0fdf4 0%, #dcfce7 100%) !important;
          color: #166534 !important;
          border-left: 4px solid var(--success-color) !important;
        }
        
        .alert-warning {
          background: linear-gradient(135deg, #fffbeb 0%, #fef3c7 100%) !important;
          color: #92400e !important;
          border-left: 4px solid var(--warning-color) !important;
        }
        
        .alert-danger {
          background: linear-gradient(135deg, #fef2f2 0%, #fecaca 100%) !important;
          color: #991b1b !important;
          border-left: 4px solid var(--danger-color) !important;
        }
        
        /* Responsive Design */
        @media (max-width: 768px) {
          .content {
            padding: 15px !important;
          }
          
          .box-body {
            padding: 15px !important;
          }
          
          .btn {
            padding: 8px 16px !important;
            font-size: 13px !important;
          }
          
          .small-box h3 {
            font-size: 24px !important;
          }
        }
        
        /* Scrollbar Styling */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }
        
        ::-webkit-scrollbar-track {
          background: var(--gray-100);
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: var(--gray-400);
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: var(--gray-500);
        }
        
        /* Animation Classes */
        .fade-in {
          animation: fadeIn 0.5s ease-in;
        }
        
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(10px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        .slide-in {
          animation: slideIn 0.3s ease-out;
        }
        
        @keyframes slideIn {
          from { transform: translateX(-10px); opacity: 0; }
          to { transform: translateX(0); opacity: 1; }
        }
      "))
    ),
    
    tabItems(
      # Tab Beranda
      tabItem(
        tabName = "beranda",
        fluidRow(
          box(
            title = "Informasi Dashboard",
            status = "primary", solidHeader = TRUE, width = 12,
            div(
              style = "padding: 20px;",
              h3("Selamat Datang di Dashboard AKSI (Analisis Kerentanan Sosial Indonesia)"),
              p("Dashboard ini dirancang untuk memantau dan menganalisis kerentanan sosial di Indonesia
                dengan pendekatan berbasis data Official Statistik. Dashboard ini juga bertujuan sebagai
                sarana untuk menyebarluaskan penelitian Nasution dkk. yang berjudul 'Revisiting Social
                Vulnerability Analysis in Indonesia' dalam bentuk yang lebih mudah dicerna oleh masyarakat awam."),
              p("Dashboard ini mengadopsi dataset yang menyediakan indikator pembangunan dan bencana dari
                511 kabupaten di Indonesia dan matriks jarak antar kabupaten. Dataset ini didapatkan dengan
                data dari berbagai bidang untuk menghasilkan analisis multidisiplin yang lebih mendalam dan
                terstruktur, khususnya kerentanan sosial dalam konteks sektor lain. Sumber data primer yang
                digunakan dalam penelitian ini adalah Survei Sosial Ekonomi Nasional (SUSENAS) 2017. Sementara
                itu, data populasi dan pertumbuhan diperoleh dari proyeksi populasi Indonesia tahun 2017."),
              p("Fitur yang tersedia:"),
              tags$ul(
                tags$li("Manajemen dan kategorisasi data"),
                tags$li("Eksplorasi data dengan statistik deskriptif, grafik, peta, dan tabel"),
                tags$li("Uji asumsi statistik (normalitas dan homogenitas)"),
                tags$li("Berbagai uji statistik inferensia"),
                tags$li("Analisis regresi linear berganda dengan uji asumsi lengkap"),
                tags$li("Download laporan dalam format PDF")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Metadata Variabel",
            status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("metadata_table")
          )
        ),
        fluidRow(
          box(
            title = "Dataset Variabel Kerentanan Sosial",
            status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("dataset_table")
          )
        )
      ),
      
      # Tab Manajemen Data
      tabItem(
        tabName = "manajemen",
        fluidRow(
          box(
            title = "Kategorisasi Data Kontinu",
            status = "primary", solidHeader = TRUE, width = 6,
            selectInput("var_kategorisasi", "Pilih Variabel:",
                        choices = variables[-1], selected = variables[2]),
            selectInput("metode_kategorisasi", "Metode Kategorisasi:",
                        choices = c("Quantile" = "quantile",
                                    "Equal Interval" = "equal"),
                        selected = "quantile"),
            actionButton("btn_kategorisasi", "Kategorisasi Data", class = "btn-primary")
          ),
          box(
            title = "Hasil Kategorisasi",
            status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("hasil_kategorisasi"),
            plotOutput("plot_kategorisasi", height = "300px")
          )
        ),
        fluidRow(
          box(
            title = "Interpretasi Kategorisasi",
            status = "warning", solidHeader = TRUE, width = 12,
            div(class = "interpretation-box", textOutput("interpretasi_kategorisasi")),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(3, downloadButton("download_kategorisasi_img", "Download Gambar (JPG)", class = "btn-info btn-sm")),
                  column(3, downloadButton("download_kategorisasi_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                  column(3, downloadButton("download_kategorisasi_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                  column(3, downloadButton("download_kategorisasi_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Statistik Deskriptif
      tabItem(
        tabName = "deskriptif",
        fluidRow(
          box(
            title = "Pilih Variabel untuk Analisis Deskriptif",
            status = "primary", solidHeader = TRUE, width = 12,
            div(
              style = "text-align: center; padding: 20px;",
              selectInput("var_deskriptif", "Pilih Variabel:",
                          choices = variables[-1], selected = variables[2]),
              br(),
              actionButton("btn_deskriptif", "Analisis", class = "btn-primary")
            )
          )
        ),
        fluidRow(
          valueBoxOutput("vbox_minimum", width = 2),
          valueBoxOutput("vbox_maksimum", width = 2),
          valueBoxOutput("vbox_median", width = 2),
          valueBoxOutput("vbox_mean", width = 2),
          valueBoxOutput("vbox_varians", width = 2),
          valueBoxOutput("vbox_kof", width = 2)
        ),
        fluidRow(
          box(
            title = "Interpretasi Statistik Deskriptif",
            status = "warning", solidHeader = TRUE, width = 12,
            div(class = "interpretation-box", textOutput("interpretasi_deskriptif")),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(4, downloadButton("download_deskriptif_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                  column(4, downloadButton("download_deskriptif_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                  column(4, downloadButton("download_deskriptif_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Grafik
      tabItem(
        tabName = "grafik",
        fluidRow(
          box(
            title = "Pilih Variabel untuk Grafik",
            status = "primary", solidHeader = TRUE, width = 4,
            selectInput("var_grafik", "Pilih Variabel:",
                        choices = variables[-1], selected = variables[2]),
            selectInput("jenis_grafik", "Jenis Grafik:",
                        choices = c("Histogram", "Box Plot", "Density Plot"),
                        selected = "Histogram"),
            actionButton("btn_grafik", "Buat Grafik", class = "btn-primary")
          ),
          box(
            title = "Grafik Visualisasi",
            status = "info", solidHeader = TRUE, width = 8,
            plotOutput("output_grafik", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Interpretasi Grafik",
            status = "warning", solidHeader = TRUE, width = 12,
            div(class = "interpretation-box", textOutput("interpretasi_grafik")),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(3, downloadButton("download_grafik_img", "Download Gambar (JPG)", class = "btn-info btn-sm")),
                  column(3, downloadButton("download_grafik_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                  column(3, downloadButton("download_grafik_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                  column(3, downloadButton("download_grafik_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Peta
      tabItem(
        tabName = "peta",
        fluidRow(
          box(
            title = "Pilih Variabel untuk Peta",
            status = "primary", solidHeader = TRUE, width = 4,
            selectInput("var_peta", "Pilih Variabel:",
                        choices = variables[-1], selected = variables[2]),
            actionButton("btn_peta", "Buat Peta", class = "btn-primary")
          ),
          box(
            title = "Peta Visualisasi",
            status = "info", solidHeader = TRUE, width = 8,
            leafletOutput("output_peta", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Interpretasi Peta",
            status = "warning", solidHeader = TRUE, width = 12,
            div(class = "interpretation-box", textOutput("interpretasi_peta")),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(3, downloadButton("download_peta_img", "Download Gambar (PNG)", class = "btn-info btn-sm")),
                  column(3, downloadButton("download_peta_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                  column(3, downloadButton("download_peta_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                  column(3, downloadButton("download_peta_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Tabel 
      tabItem(
        tabName = "tabel",
        fluidRow(
          box(
            title = "Pilih Variabel untuk Tabel",
            status = "primary", solidHeader = TRUE, width = 4,
            selectInput("var_tabel", "Pilih Variabel:",
                        choices = variables[-1], selected = variables[2]),
            actionButton("btn_tabel", "Tampilkan Tabel", class = "btn-primary")
          ),
          box(
            title = "Tabel Data Variabel Terpilih",
            status = "info", solidHeader = TRUE, width = 8,
            DT::dataTableOutput("output_tabel")
          )
        ),
        fluidRow(
          box(
            title = "Interpretasi Tabel",
            status = "warning", solidHeader = TRUE, width = 12,
            div(class = "interpretation-box", textOutput("interpretasi_tabel")),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(4, downloadButton("download_tabel_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                  column(4, downloadButton("download_tabel_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                  column(4, downloadButton("download_tabel_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Uji Asumsi
      tabItem(
        tabName = "uji",
        fluidRow(
          tabBox(
            title = "Uji Asumsi",
            id = "tabset_uji",
            width = 12,
            tabPanel(
              "Uji Normalitas",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan Uji Normalitas"),
                    selectInput("var_normalitas", "Pilih Variabel:",
                                choices = variables[-1], selected = variables[2]),
                    selectInput("metode_normalitas", "Metode Uji:",
                                choices = c("Shapiro-Wilk" = "shapiro",
                                            "Kolmogorov-Smirnov" = "ks"),
                                selected = "shapiro"),
                    actionButton("btn_normalitas", "Uji Normalitas", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: Data berdistribusi normal"),
                      p("H1: Data tidak berdistribusi normal"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil Uji Normalitas:"),
                    verbatimTextOutput("hasil_normalitas")
                  )
                )
              )
            ),
            tabPanel(
              "Uji Homogenitas",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan Uji Homogenitas"),
                    selectInput("var_homogenitas", "Pilih Variabel:",
                                choices = variables[-1], selected = variables[2]),
                    selectInput("metode_homogenitas", "Metode Uji:",
                                choices = c("Levene Test" = "levene",
                                            "Bartlett Test" = "bartlett"),
                                selected = "levene"),
                    actionButton("btn_homogenitas", "Uji Homogenitas", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: Varians kelompok sama (homogen)"),
                      p("H1: Varians kelompok tidak sama"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil Uji Homogenitas:"),
                    verbatimTextOutput("hasil_homogenitas")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Interpretasi Uji Asumsi",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            div(
              class = "interpretation-box",
              h5("Interpretasi:"),
              textOutput("interpretasi_uji_asumsi")
            ),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(3, downloadButton("download_uji_normal_report", "Download Laporan Normalitas (PDF)", class = "btn-success btn-sm")),
                  column(3, downloadButton("download_uji_homo_report", "Download Laporan Homogenitas (PDF)", class = "btn-success btn-sm")),
                  column(3, downloadButton("download_uji_normal_interp", "Download Interpretasi Normalitas (PDF)", class = "btn-warning btn-sm")),
                  column(3, downloadButton("download_uji_homo_interp", "Download Interpretasi Homogenitas (PDF)", class = "btn-warning btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Uji Beda Rata-Rata
      tabItem(
        tabName = "rata",
        fluidRow(
          tabBox(
            title = "Uji Beda Rata-Rata",
            id = "tabset_rata",
            width = 12,
            tabPanel(
              "Uji Satu Kelompok",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan Uji t Satu Sampel"),
                    selectInput("var_rata_satu", "Pilih Variabel:",
                                choices = variables[-1], selected = variables[2]),
                    numericInput("nilai_hipotesis_rata", "Nilai Hipotesis (μ0):", value = 0),
                    selectInput("alternative_rata_satu", "Hipotesis Alternatif:",
                                choices = c("Two-sided" = "two.sided",
                                            "Greater than" = "greater",
                                            "Less than" = "less"),
                                selected = "two.sided"),
                    actionButton("btn_rata_satu", "Lakukan Uji t Satu Sampel", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: μ = μ0"),
                      p("H1: μ ≠ μ0 (two-sided)"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil Uji t Satu Sampel:"),
                    verbatimTextOutput("hasil_rata_satu")
                  )
                )
              )
            ),
            tabPanel(
              "Uji Dua Kelompok",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan Uji t Dua Kelompok"),
                    selectInput("var_rata_dua1", "Pilih Variabel Pertama:",
                                choices = variables[-1], selected = variables[2]),
                    selectInput("var_rata_dua2", "Pilih Variabel Kedua:",
                                choices = variables[-1], selected = variables[3]),
                    selectInput("metode_rata_dua", "Metode Uji:",
                                choices = c("Uji t Dua Sampel" = "t_two",
                                            "Uji t Berpasangan" = "t_paired"),
                                selected = "t_two"),
                    checkboxInput("var_equal", "Asumsi varians sama", value = TRUE),
                    actionButton("btn_rata_dua", "Lakukan Uji", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: μ1 = μ2"),
                      p("H1: μ1 ≠ μ2"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil Uji t Dua Kelompok:"),
                    verbatimTextOutput("hasil_rata_dua")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Interpretasi Uji Beda Rata-Rata",
            status = "warning", solidHeader = TRUE, width = 12,
            div(class = "interpretation-box", textOutput("interpretasi_rata")),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(3, downloadButton("download_rata_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                  column(3, downloadButton("download_rata_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                  column(3, downloadButton("download_rata_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Uji Proporsi
      tabItem(
        tabName = "proporsi",
        fluidRow(
          tabBox(
            title = "Uji Proporsi",
            id = "tabset_proporsi",
            width = 12,
            tabPanel(
              "Uji Proporsi Satu Kelompok",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan Uji Proporsi Satu Kelompok"),
                    selectInput("var_proporsi_satu", "Pilih Variabel:",
                                choices = variables[-1], selected = variables[2]),
                    numericInput("nilai_batas_satu", "Nilai Batas untuk Kategorisasi:", value = 0),
                    numericInput("proporsi_hipotesis_satu", "Proporsi Hipotesis (p0):",
                                 value = 0.5, min = 0, max = 1, step = 0.01),
                    selectInput("alternative_proporsi_satu", "Hipotesis Alternatif:",
                                choices = c("Two-sided" = "two.sided",
                                            "Greater than" = "greater",
                                            "Less than" = "less"),
                                selected = "two.sided"),
                    actionButton("btn_proporsi_satu", "Lakukan Uji", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: p = p0"),
                      p("H1: p ≠ p0 (two-sided)"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil Uji Proporsi Satu Kelompok:"),
                    verbatimTextOutput("hasil_proporsi_satu")
                  )
                )
              )
            ),
            tabPanel(
              "Uji Proporsi Dua Kelompok",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan Uji Proporsi Dua Kelompok"),
                    selectInput("var_proporsi_dua1", "Pilih Variabel Pertama:",
                                choices = variables[-1], selected = variables[2]),
                    selectInput("var_proporsi_dua2", "Pilih Variabel Kedua:",
                                choices = variables[-1], selected = variables[3]),
                    numericInput("nilai_batas_dua", "Nilai Batas untuk Kategorisasi:", value = 0),
                    actionButton("btn_proporsi_dua", "Lakukan Uji", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: p1 = p2"),
                      p("H1: p1 ≠ p2"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil Uji Proporsi Dua Kelompok:"),
                    verbatimTextOutput("hasil_proporsi_dua")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Interpretasi Uji Proporsi",
            status = "warning", solidHeader = TRUE, width = 12,
            div(class = "interpretation-box", textOutput("interpretasi_proporsi")),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(3, downloadButton("download_proporsi_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                  column(3, downloadButton("download_proporsi_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                  column(3, downloadButton("download_proporsi_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Uji Variance
      tabItem(
        tabName = "variance",
        fluidRow(
          tabBox(
            title = "Uji Variance",
            id = "tabset_variance",
            width = 12,
            tabPanel(
              "Uji Variance Satu Kelompok",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan Uji Variance Satu Kelompok (Chi-square test)"),
                    selectInput("var_variance_satu", "Pilih Variabel:",
                                choices = variables[-1], selected = variables[2]),
                    numericInput("variance_hipotesis", "Variance Hipotesis (σ²0):", value = 1, min = 0.01),
                    actionButton("btn_variance_satu", "Lakukan Uji", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: σ² = σ²0"),
                      p("H1: σ² ≠ σ²0"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil Uji Variance Satu Kelompok:"),
                    verbatimTextOutput("hasil_variance_satu")
                  )
                )
              )
            ),
            tabPanel(
              "Uji Variance Dua Kelompok",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan Uji Variance Dua Kelompok"),
                    selectInput("var_variance_dua1", "Pilih Variabel Pertama:",
                                choices = variables[-1], selected = variables[2]),
                    selectInput("var_variance_dua2", "Pilih Variabel Kedua:",
                                choices = variables[-1], selected = variables[3]),
                    selectInput("metode_variance_dua", "Metode Uji:",
                                choices = c("F Test" = "ftest",
                                            "Levene Test" = "levene"),
                                selected = "ftest"),
                    actionButton("btn_variance_dua", "Lakukan Uji", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: σ²1 = σ²2"),
                      p("H1: σ²1 ≠ σ²2"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil Uji Variance Dua Kelompok:"),
                    verbatimTextOutput("hasil_variance_dua")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Interpretasi Uji Variance",
            status = "warning", solidHeader = TRUE, width = 12,
            div(class = "interpretation-box", textOutput("interpretasi_variance")),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(3, downloadButton("download_variance_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                  column(3, downloadButton("download_variance_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                  column(3, downloadButton("download_variance_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Uji ANOVA
      tabItem(
        tabName = "anova",
        fluidRow(
          tabBox(
            title = "Uji ANOVA",
            id = "tabset_anova",
            width = 12,
            tabPanel(
              "ANOVA Satu Arah",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan ANOVA Satu Arah"),
                    selectInput("var_anova_satu_y", "Variabel Dependen:",
                                choices = variables[-1], selected = variables[2]),
                    selectInput("var_anova_satu_group", "Variabel Pengelompokan:",
                                choices = variables[-1], selected = variables[3]),
                    numericInput("n_groups_satu", "Jumlah Kelompok:", value = 3, min = 2, max = 5),
                    actionButton("btn_anova_satu", "Lakukan ANOVA Satu Arah", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: μ1 = μ2 = ... = μk"),
                      p("H1: Setidaknya satu μi berbeda"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil ANOVA Satu Arah:"),
                    verbatimTextOutput("hasil_anova_satu")
                  )
                )
              )
            ),
            tabPanel(
              "ANOVA Dua Arah",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan ANOVA Dua Arah"),
                    selectInput("var_anova_dua_y", "Variabel Dependen:",
                                choices = variables[-1], selected = variables[2]),
                    selectInput("var_anova_dua_group1", "Faktor Pertama:",
                                choices = variables[-1], selected = variables[3]),
                    selectInput("var_anova_dua_group2", "Faktor Kedua:",
                                choices = variables[-1], selected = variables[4]),
                    numericInput("n_groups_dua1", "Jumlah Level Faktor 1:", value = 3, min = 2, max = 4),
                    numericInput("n_groups_dua2", "Jumlah Level Faktor 2:", value = 3, min = 2, max = 4),
                    actionButton("btn_anova_dua", "Lakukan ANOVA Dua Arah", class = "btn-primary"),
                    br(), br(),
                    div(
                      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
                      h5("Hipotesis:"),
                      p("H0: Tidak ada efek faktor"),
                      p("H1: Ada efek faktor"),
                      p("α = 0.05")
                    )
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "interpretation-box",
                    h5("Hasil ANOVA Dua Arah:"),
                    verbatimTextOutput("hasil_anova_dua")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Interpretasi Uji ANOVA",
            status = "warning", solidHeader = TRUE, width = 12,
            div(class = "interpretation-box", textOutput("interpretasi_anova")),
            div(class = "download-buttons",
                h5("Download Laporan:"),
                fluidRow(
                  column(3, downloadButton("download_anova_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                  column(3, downloadButton("download_anova_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                  column(3, downloadButton("download_anova_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                )
            )
          )
        )
      ),
      
      # Tab Regresi 
      tabItem(
        tabName = "regresi",
        fluidRow(
          tabBox(
            title = "Analisis Regresi Linear Berganda",
            id = "tabset_regresi",
            width = 12,
            tabPanel(
              "Model Regresi",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "variable-selection",
                    h5("Pengaturan Regresi Linear Berganda"),
                    selectInput("var_regresi_y", "Variabel Dependen (Y):",
                                choices = variables[7:16], selected = variables[2]),
                    checkboxGroupInput("var_regresi_x", "Variabel Independen (X):",
                                       choices = variables[-1],
                                       selected = variables[3:5]),
                    checkboxInput("include_intercept", "Sertakan Intercept", value = TRUE),
                    actionButton("btn_regresi", "Analisis Regresi", class = "btn-primary")
                  )
                ),
                column(
                  width = 8,
                  div(
                    class = "result-box",
                    h5("Hasil Regresi Linear Berganda:"),
                    verbatimTextOutput("hasil_regresi")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  div(
                    class = "result-box",
                    h5("Plot Diagnostik:"),
                    plotOutput("plot_diagnostik", height = "400px")
                  )
                ),
                column(
                  width = 6,
                  div(
                    class = "interpretation-box",
                    h5("Interpretasi Regresi:"),
                    textOutput("interpretasi_regresi")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  div(class = "download-buttons",
                      h5("Download Laporan Model Regresi:"),
                      fluidRow(
                        column(3, downloadButton("download_regresi_img", "Download Plot (JPG)", class = "btn-info btn-sm")),
                        column(3, downloadButton("download_regresi_report", "Download Laporan (PDF)", class = "btn-success btn-sm")),
                        column(3, downloadButton("download_regresi_interp", "Download Interpretasi (PDF)", class = "btn-warning btn-sm")),
                        column(3, downloadButton("download_regresi_all", "Download Semua (PDF)", class = "btn-primary btn-sm"))
                      )
                  )
                )
              )
            ),
            tabPanel(
              "Uji Asumsi Regresi",
              fluidRow(
                column(
                  width = 12,
                  div(
                    class = "assumption-box",
                    h4("Uji Asumsi Model Regresi"),
                    p("Sebelum menginterpretasi hasil regresi, penting untuk memvalidasi asumsi-asumsi berikut:"),
                    tags$ul(
                      tags$li("Normalitas residual"),
                      tags$li("Homoskedastisitas (varians residual konstan)"),
                      tags$li("Non-autokorelasi (independensi residual)"),
                      tags$li("Non-multikolinearitas (tidak ada korelasi tinggi antar prediktor)")
                    ),
                    actionButton("btn_uji_asumsi_regresi", "Uji Semua Asumsi", class = "btn-warning btn-lg")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 6,
                  div(
                    class = "result-box",
                    h5("1. Uji Normalitas Residual (Shapiro-Wilk)"),
                    verbatimTextOutput("hasil_normalitas_residual"),
                    br(),
                    h5("2. Uji Homoskedastisitas (Breusch-Pagan)"),
                    verbatimTextOutput("hasil_homoskedastisitas")
                  )
                ),
                column(
                  width = 6,
                  div(
                    class = "result-box",
                    h5("3. Uji Non-Autokorelasi (Durbin-Watson)"),
                    verbatimTextOutput("hasil_autokorelasi"),
                    br(),
                    h5("4. Uji Non-Multikolinearitas (VIF)"),
                    verbatimTextOutput("hasil_multikolinearitas")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  div(
                    class = "interpretation-box",
                    h5("Interpretasi Uji Asumsi:"),
                    textOutput("interpretasi_asumsi_regresi")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  div(class = "download-buttons",
                      h5("Download Laporan Uji Asumsi Regresi:"),
                      fluidRow(
                        column(4, downloadButton("download_asumsi_regresi_report", "Download Laporan Uji Asumsi (PDF)", class = "btn-success btn-sm")),
                        column(4, downloadButton("download_asumsi_regresi_interp", "Download Interpretasi Asumsi (PDF)", class = "btn-warning btn-sm")),
                        column(4, downloadButton("download_asumsi_regresi_all", "Download Lengkap Asumsi (PDF)", class = "btn-primary btn-sm"))
                      )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values untuk menyimpan data hasil analisis
  values <- reactiveValues()
  
  # Dataset table untuk beranda
  output$dataset_table <- DT::renderDataTable({
    data_variabel
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Metadata table
  output$metadata_table <- DT::renderDataTable({
    descriptions <- c(
      "DISTRICTCODE" = "Kode wilayah/kabupaten",
      "CHILDREN" = "Persentase penduduk usia di bawah lima tahun",
      "FEMALE" = "Persentase penduduk perempuan",
      "ELDERLY" = "Persentase penduduk usia 65 tahun dan kelebihan populasi",
      "FHEAD" = "Persentase rumah tangga dengan kepala rumah tangga perempuan",
      "FAMILYSIZE" = "Rata-rata jumlah anggota rumah tangga di satu distrik",
      "NOELECTRIC" = "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan",
      "LOWEDU" = "Persentase penduduk usia 15 tahun ke atas dengan pendidikan rendah",
      "GROWTH" = "Persentase perubahan populasi",
      "POVERTY" = "Persentase penduduk miskin",
      "ILLITERATE" = "Persentase penduduk yang tidak bisa membaca dan menulis",
      "NOTRAINING" = "Persentase rumah tangga yang tidak mendapatkan pelatihan bencana",
      "DPRONE" = "Persentase rumah tangga yang tinggal di daerah rawan bencana",
      "RENTED" = "Persentase rumah tangga yang menyewa rumah",
      "NOSEWER" = "Persentase rumah tangga yang tidak memiliki sistem drainase",
      "TAPWATER" = "Persentase rumah tangga yang menggunakan air ledeng",
      "POPULATION" = "Jumlah Penduduk"
    )
    table_variables <- variables
    
    mapped_descriptions <- descriptions[table_variables]
    
    metadata_df <- data.frame(
      `No.` = 1:length(table_variables),
      Variabel = table_variables,
      Deskripsi = mapped_descriptions,
      Tipe = rep("Numerik", length(table_variables))
    )
    
    DT::datatable(
      metadata_df,
      options = list(pageLength = 17, scrollX = TRUE),
      rownames = FALSE 
    )
  })
  
  # Kategorisasi data
  observeEvent(input$btn_kategorisasi, {
    req(input$var_kategorisasi)
    
    var_data <- data_variabel[[input$var_kategorisasi]]
    
    if(input$metode_kategorisasi == "quantile") {
      breaks <- quantile(var_data, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
      categories <- cut(var_data, breaks = breaks, labels = c("Rendah", "Sedang", "Tinggi"), include.lowest = TRUE)
    } else if(input$metode_kategorisasi == "equal") {
      breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length.out = 4)
      categories <- cut(var_data, breaks = breaks, labels = c("Rendah", "Sedang", "Tinggi"), include.lowest = TRUE)
    }
    
    result_table <- table(categories)
    
    # Simpan hasil untuk download
    values$kategorisasi_result <- result_table
    values$kategorisasi_var <- input$var_kategorisasi
    values$kategorisasi_method <- input$metode_kategorisasi
    values$kategorisasi_breaks <- breaks
    
    output$hasil_kategorisasi <- renderText({
      paste("Hasil Kategorisasi:\n",
            "Rendah:", result_table[1], "daerah\n",
            "Sedang:", result_table[2], "daerah\n", 
            "Tinggi:", result_table[3], "daerah\n\n",
            "Batas Kategori:\n",
            "Rendah: ≤", round(breaks[2], 3), "\n",
            "Sedang:", round(breaks[2], 3), "-", round(breaks[3], 3), "\n",
            "Tinggi: >", round(breaks[3], 3))
    })
    
    output$plot_kategorisasi <- renderPlot({
      barplot(result_table, main = paste("Distribusi Kategori", input$var_kategorisasi),
              col = c("green", "yellow", "red"), ylab = "Jumlah Daerah",
              xlab = "Kategori")
    })
    
    output$interpretasi_kategorisasi <- renderText({
      total_daerah <- sum(result_table)
      pct_rendah <- round(result_table[1]/total_daerah * 100, 1)
      pct_sedang <- round(result_table[2]/total_daerah * 100, 1)
      pct_tinggi <- round(result_table[3]/total_daerah * 100, 1)
      
      paste("Variabel", input$var_kategorisasi, "telah dikategorisasi menjadi 3 tingkat menggunakan metode",
            input$metode_kategorisasi, ". Dari", total_daerah, "daerah yang dianalisis,",
            pct_rendah, "% daerah termasuk dalam kategori rendah,", pct_sedang, "% daerah berada pada kategori sedang, dan",
            pct_tinggi, "% daerah merupakan kategori tinggi. Kategorisasi ini dapat membantu identifikasi daerah",
            "yang memerlukan perhatian khusus dalam program pengurangan kerentanan sosial. Dengan informasi ini, 
            pemerintah dan pemangku kebijakan bisa lebih mudah menetapkan prioritas intervensi untuk meningkatkan kualitas hidup di daerah yang paling membutuhkan.")
    })
  })
  
  # Statistik Deskriptif
  observeEvent(input$btn_deskriptif, {
    req(input$var_deskriptif)
    
    var_data <- data_variabel[[input$var_deskriptif]]
    
    # Hitung statistik
    min_val <- min(var_data, na.rm = TRUE)
    max_val <- max(var_data, na.rm = TRUE)
    median_val <- median(var_data, na.rm = TRUE)
    mean_val <- mean(var_data, na.rm = TRUE)
    var_val <- var(var_data, na.rm = TRUE)
    sd_val <-sd(var_data, na.rm = TRUE)
    kof_val <- sd_val/mean_val * 100
    
    # Simpan hasil untuk download
    values$deskriptif_result <- data.frame(
      Statistik = c("Minimum", "Maksimum", "Median", "Mean", "Varians"),
      Nilai = c(min_val, max_val, median_val, mean_val, var_val)
    )
    values$deskriptif_var <- input$var_deskriptif
    
    # Render valueBox untuk setiap statistik
    output$vbox_minimum <- renderValueBox({
      valueBox(
        value = round(min_val, 4),
        subtitle = "Minimum",
        icon = icon("arrow-down"),
        color = "red"
      )
    })
    
    output$vbox_maksimum <- renderValueBox({
      valueBox(
        value = round(max_val, 4),
        subtitle = "Maksimum", 
        icon = icon("arrow-up"),
        color = "green"
      )
    })
    
    output$vbox_median <- renderValueBox({
      valueBox(
        value = round(median_val, 4),
        subtitle = "Median",
        icon = icon("divide"),
        color = "blue"
      )
    })
    
    output$vbox_mean <- renderValueBox({
      valueBox(
        value = round(mean_val, 4),
        subtitle = "Mean (Rata-rata)",
        icon = icon("calculator"),
        color = "purple"
      )
    })
    
    output$vbox_varians <- renderValueBox({
      valueBox(
        value = round(var_val, 4),
        subtitle = "Varians",
        icon = icon("expand-arrows-alt"),
        color = "orange"
      )
    })
    
    output$vbox_kof <- renderValueBox({
      valueBox(
        value = round(kof_val, 4),
        subtitle = "Koefisien Variasi",
        icon = icon("percent"),
        color = "yellow"
      )
    })
    
    output$interpretasi_deskriptif <- renderText({
      mean_val <- round(mean(var_data, na.rm = TRUE), 3)
      median_val <- round(median(var_data, na.rm = TRUE), 3)
      sd_val <- round(sd(var_data, na.rm = TRUE), 3)
      cv <- round(sd_val/mean_val * 100, 1)
      
      paste("Variabel", input$var_deskriptif, "memiliki rata-rata", mean_val, "dengan standar deviasi", sd_val, 
            "yang menggambarkan seberapa besar penyebaran data dari nilai rata-rata.\n",
            "Nilai median adalah", median_val, "menunjukkan nilai tengah dari distribusi data.\n",
            "Koefisien variasi sebesar", cv, "% menunjukkan bahwa variasi antar wilayah tergolong",
            ifelse(cv < 20, "rendah,", ifelse(cv < 50, "sedang,", "tinggi,")),
            "artinya data", ifelse(cv < 20, "cukup seragam", ifelse(cv < 50, "bervariasi sedang", "sangat bervariasi")), ".\n",
            "Selisih antara rata-rata dan median", ifelse(abs(mean_val - median_val) < sd_val/2, 
                                                          "tidak terlalu besar, sehingga distribusi data cenderung simetris.", "cukup besar, yang bisa mengindikasikan adanya kemencengan (skewness) dalam data."))
    })
  })
  
  # Grafik
  observeEvent(input$btn_grafik, {
    req(input$var_grafik)
    
    # Simpan parameter untuk download
    values$grafik_var <- input$var_grafik
    values$grafik_type <- input$jenis_grafik
    
    output$output_grafik <- renderPlot({
      var_data <- data_variabel[[input$var_grafik]]
      
      if(input$jenis_grafik == "Histogram") {
        hist(var_data, main = paste("Histogram", input$var_grafik),
             xlab = input$var_grafik, col = "lightblue", breaks = 20,
             ylab = "Frekuensi")
        abline(v = mean(var_data, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
        legend("topright", legend = "Mean", col = "red", lty = 2, lwd = 2)
      } else if(input$jenis_grafik == "Box Plot") {
        boxplot(var_data, main = paste("Box Plot", input$var_grafik),
                ylab = input$var_grafik, col = "lightgreen")
        points(1, mean(var_data, na.rm = TRUE), col = "red", pch = 19, cex = 1.5)
        legend("topright", legend = "Mean", col = "red", pch = 19)
      } else if(input$jenis_grafik == "Density Plot") {
        plot(density(var_data, na.rm = TRUE), main = paste("Density Plot", input$var_grafik),
             xlab = input$var_grafik, col = "red", lwd = 2)
        polygon(density(var_data, na.rm = TRUE), col = rgb(1,0,0,0.3))
        abline(v = mean(var_data, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
        abline(v = median(var_data, na.rm = TRUE), col = "green", lwd = 2, lty = 2)
        legend("topright", legend = c("Mean", "Median"), col = c("blue", "green"), lty = 2, lwd = 2)
      }
    })
    
    output$interpretasi_grafik <- renderText({
      var_data <- data_variabel[[input$var_grafik]]
      
      if(input$jenis_grafik == "Histogram") {
        paste("Histogram variabel", input$var_grafik,
              "menampilkan pola distribusi frekuensi nilai-nilai yang diamati.",
              "Bentuk histogram membantu mengenali apakah data menyebar secara simetris, condong ke kiri atau kanan (skewed), atau bahkan memiliki lebih dari satu puncak (multimodal).",
              "Garis merah vertikal menunjukkan nilai rata-rata dari data.")
      } else if(input$jenis_grafik == "Box Plot") {
        q1 <- quantile(var_data, 0.25, na.rm = TRUE)
        q3 <- quantile(var_data, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        outliers <- sum(var_data < (q1 - 1.5*iqr) | var_data > (q3 + 1.5*iqr), na.rm = TRUE)
        paste("Box plot variabel", input$var_grafik,
              "memberikan gambaran menyeluruh tentang persebaran data melalui nilai kuartil dan pencilan.",
              "Garis tengah dalam kotak menunjukkan median, sedangkan panjang kotak mencerminkan rentang antarkuartil (IQR).",
              "Ditemukan", outliers, "data yang teridentifikasi sebagai outlier potensial — titik-titik di luar rentang normal.",
              "Titik merah pada grafik menandai rata-rata.")
      } else {
        paste("Density plot variabel", input$var_grafik,
              "menunjukkan gambaran halus dari distribusi probabilitas data.",
              "Bentuk kurva memberi indikasi apakah data menyebar secara normal, miring ke kiri/kanan, atau menunjukkan gejala multimodalitas.",
              "Garis biru menandakan nilai rata-rata, sedangkan garis hijau menunjukkan median.",
              "Perbedaan letak garis ini bisa mengungkap simetri atau kemencengan distribusi.")
      }
    })
  })
  
  # Peta
  observeEvent(input$btn_peta, {
    req(input$var_peta)
    
    # Simpan parameter untuk download
    values$peta_var <- input$var_peta
    
    output$output_peta <- renderLeaflet({
      tryCatch({
        
        # Join data berdasarkan districtcode (CSV) = kodeprkab (GeoJSON)
        if("kodeprkab" %in% names(map_data) && "districtcode" %in% names(data_variabel)) {
          # Pastikan kedua kolom memiliki tipe data yang sama
          map_data$kodeprkab <- as.character(map_data$kodeprkab)
          data_variabel$districtcode <- as.character(data_variabel$districtcode)
          
          # Join data
          map_data <- map_data %>%
            left_join(data_variabel, by = c("kodeprkab" = "districtcode"))
          
          # Gunakan nmkab sebagai nama daerah
          region_names <- if("nmkab" %in% names(map_data)) {
            map_data$nmkab
          } else {
            map_data$kodeprkab  # fallback ke kodeprkab jika nmkab tidak ada
          }
        } else {
          # Gabungkan berdasarkan urutan
          region_names <- if("nmkab" %in% names(map_data)) {
            map_data$nmkab
          } else {
            # Fallback jika 'nmkab' tidak ada, gunakan kode sebagai nama.
            map_data$kodeprkab 
          }
        }
        
        pal <- colorNumeric(
          palette = "RdYlBu",
          domain = map_data[[input$var_peta]],
          reverse = TRUE,
          na.color = "#808080"
        )
        
        var_values <- map_data[[input$var_peta]]
        
        popup_content <- paste0(
          "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.4;'>",
          "<h4 style='margin: 0 0 10px 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>",
          "<i class='fa fa-map-marker'></i> Informasi Kabupaten/Kota</h4>",
          "<table style='width: 100%; border-collapse: collapse;'>",
          "<tr style='background-color: #f8f9fa;'>",
          "<td style='padding: 8px; font-weight: bold; border: 1px solid #dee2e6;'>Nama Kabupaten:</td>",
          "<td style='padding: 8px; border: 1px solid #dee2e6; color: #2c3e50; font-weight: bold;'>", region_names, "</td>",
          "</tr>",
          "<tr>",
          "<td style='padding: 8px; font-weight: bold; border: 1px solid #dee2e6;'>Kode Daerah:</td>",
          "<td style='padding: 8px; border: 1px solid #dee2e6;'>", 
          ifelse("kodeprkab" %in% names(map_data), map_data$kodeprkab, "N/A"), "</td>",
          "</tr>",
          "<tr style='background-color: #f8f9fa;'>",
          "<td style='padding: 8px; font-weight: bold; border: 1px solid #dee2e6;'>", input$var_peta, ":</td>",
          "<td style='padding: 8px; border: 1px solid #dee2e6; color: #e74c3c; font-weight: bold;'>", 
          round(var_values, 4), "</td>",
          "</tr>",
          "<tr>",
          "<td style='padding: 8px; font-weight: bold; border: 1px solid #dee2e6;'>Kategori:</td>",
          "<td style='padding: 8px; border: 1px solid #dee2e6;'>", 
          ifelse(is.na(var_values), "<span style='color: #95a5a6;'>Data tidak tersedia</span>",
                 ifelse(var_values > quantile(var_values, 0.67, na.rm = TRUE),
                        "<span style='color: #e74c3c; font-weight: bold;'>Tinggi</span>",
                        ifelse(var_values > quantile(var_values, 0.33, na.rm = TRUE),
                               "<span style='color: #f39c12; font-weight: bold;'>Sedang</span>",
                               "<span style='color: #27ae60; font-weight: bold;'>Rendah</span>"))), "</td>",
          "</tr>",
          "</table>",
          "<p style='margin: 10px 0 0 0; font-size: 12px; color: #7f8c8d; text-align: center;'>",
          "<i>Klik untuk melihat detail lengkap</i></p>",
          "</div>"
        )
        
        # Buat peta dan simpan ke values untuk screenshot
        peta_leaflet <- leaflet(map_data) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(get(input$var_peta)),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "2",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 4,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            popup = popup_content,
            popupOptions = popupOptions(
              maxWidth = 350,
              closeButton = TRUE,
              closeOnClick = TRUE
            ),
            label = ~paste(region_names, ":", round(get(input$var_peta), 3)),
            labelOptions = labelOptions(
              style = list(
                "font-weight" = "bold",
                "padding" = "6px 12px",
                "background-color" = "rgba(255,255,255,0.9)",
                "border" = "2px solid #3498db",
                "border-radius" = "5px",
                "box-shadow" = "0 2px 5px rgba(0,0,0,0.2)"
              ),
              textsize = "14px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = pal,
            values = ~get(input$var_peta),
            opacity = 0.8,
            title = HTML(paste0("<strong>", input$var_peta, "</strong><br><small>Klik area untuk detail</small>")),
            position = "bottomright",
            labFormat = labelFormat(
              suffix = "",
              between = " - ",
              digits = 3
            )
          )
        
        # Simpan peta untuk screenshot
        values$current_map <- peta_leaflet
        
        return(peta_leaflet)
        
      }, error = function(e) {
        # Fallback jika file GeoJSON tidak ditemukan
        showNotification(
          paste("File GeoJSON tidak ditemukan:", e$message,
                "Menggunakan visualisasi alternatif."),
          type = "warning",
          duration = 5
        )
        
        # Gunakan visualisasi grid sebagai fallback
        n_rows <- nrow(data_variabel)
        grid_size <- ceiling(sqrt(n_rows))
        lat_seq <- seq(-6.5, -6.0, length.out = grid_size)
        lng_seq <- seq(106.5, 107.0, length.out = grid_size)
        
        coords_grid <- expand.grid(lat = lat_seq, lng = lng_seq)[1:n_rows, ]
        coords_data <- data.frame(
          lat = coords_grid$lat,
          lng = coords_grid$lng,
          value = data_variabel[[input$var_peta]],
          region = if("districtcode" %in% names(data_variabel)) {
            data_variabel$districtcode
          } else {
            data_variabel[[1]]
          }
        )
        
        # Popup content untuk fallback
        popup_content_fallback <- paste0(
          "<div style='font-family: Arial, sans-serif; font-size: 14px;'>",
          "<h4 style='margin: 0 0 10px 0; color: #2c3e50;'>Informasi Daerah</h4>",
          "<p><strong>Kode Daerah:</strong> ", coords_data$region, "</p>",
          "<p><strong>", input$var_peta, ":</strong> <span style='color: #e74c3c; font-weight: bold;'>", 
          round(coords_data$value, 4), "</span></p>",
          "<p><strong>Kategori:</strong> ", 
          ifelse(coords_data$value > quantile(coords_data$value, 0.67, na.rm = TRUE),
                 "<span style='color: #e74c3c;'>Tinggi</span>",
                 ifelse(coords_data$value > quantile(coords_data$value, 0.33, na.rm = TRUE),
                        "<span style='color: #f39c12;'>Sedang</span>",
                        "<span style='color: #27ae60;'>Rendah</span>")), "</p>",
          "</div>"
        )
        
        fallback_map <- leaflet(coords_data) %>%
          addTiles() %>%
          addCircleMarkers(
            ~lng, ~lat,
            radius = ~pmax(8, pmin(25, sqrt(abs(value)) * 4)),
            color = ~colorNumeric("RdYlBu", value, reverse = TRUE)(value),
            fillOpacity = 0.8,
            stroke = TRUE,
            weight = 3,
            popup = popup_content_fallback,
            popupOptions = popupOptions(
              maxWidth = 250,
              closeButton = TRUE
            ),
            label = ~paste("Kode:", region, "| Nilai:", round(value, 3)),
            labelOptions = labelOptions(
              style = list("font-weight" = "bold", "padding" = "6px 12px"),
              textsize = "14px"
            )
          ) %>%
          addLegend("bottomright",
                    pal = colorNumeric("RdYlBu", coords_data$value, reverse = TRUE),
                    values = ~value,
                    title = HTML(paste0("<strong>", input$var_peta, "</strong>")),
                    opacity = 0.8)
        
        # Simpan fallback map untuk screenshot
        values$current_map <- fallback_map
        
        return(fallback_map)
      })
    })
    
    output$interpretasi_peta <- renderText({
      paste("Peta ini menampilkan persebaran spasial variabel", input$var_peta,
            "di seluruh kabupaten/kota di Indonesia.",
            "Semakin gelap warna suatu wilayah, semakin tinggi nilai variabel tersebut.",
            "Arahkan kursor (hover) ke wilayah tertentu untuk melihat ringkasan cepat, 
            atau klik area tersebut untuk mendapatkan informasi lebih detail.")
    })
  })
  
  # Tabel
  observeEvent(input$btn_tabel, {
    req(input$var_tabel)
    
    # Simpan parameter untuk download
    values$tabel_var <- input$var_tabel
    
    output$output_tabel <- DT::renderDataTable({
      tabel_data <- data.frame(
        "Kode Daerah" = data_lengkap$DISTRICTCODE,
        "Nama Daerah" = data_lengkap$nmkab,
        "Nilai" = data_variabel[[input$var_tabel]]
      )
      
      names(tabel_data)[3] <- input$var_tabel
      
      tabel_data <- tabel_data[order(tabel_data[[3]], decreasing = TRUE), ]
      
      tabel_data <- cbind("No" = 1:nrow(tabel_data), tabel_data)
      
      values$tabel_result <- tabel_data
      
      DT::datatable(
        tabel_data,
        rownames = FALSE,  
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = c(0, 3)))
        )
      )
    })
    
    output$interpretasi_tabel <- renderText({
      var_data <- data_variabel[[input$var_tabel]]
      n_data <- length(var_data[!is.na(var_data)])
      mean_val <- round(mean(var_data, na.rm = TRUE), 3)
      
      # Identifikasi daerah dengan nilai tertinggi dan terendah
      max_idx <- which.max(var_data)
      min_idx <- which.min(var_data)
      max_region <- data_lengkap[[18]][max_idx]
      min_region <- data_lengkap[[18]][min_idx]
      
      paste("Tabel ini menampilkan peringkat", n_data, "daerah berdasarkan variabel", input$var_tabel, ".",
            "Daerah dengan nilai tertinggi adalah", max_region, 
            "menunjukkan kondisi paling menonjol dalam aspek ini.",
            "Sementara itu, daerah dengan nilai terendah adalah", min_region, 
            "yang mungkin membutuhkan perhatian lebih.",
            "Rata-rata nilai secara keseluruhan adalah", mean_val, ".",
            "Informasi ini dapat dimanfaatkan untuk menyusun prioritas intervensi kebijakan secara lebih tepat sasaran.")
      
    })
  })
  
  # Uji Normalitas
  observeEvent(input$btn_normalitas, {
    req(input$var_normalitas)
    
    var_data <- data_variabel[[input$var_normalitas]]
    var_data <- var_data[!is.na(var_data)]
    
    # Simpan parameter untuk download
    values$uji_normal_var <- input$var_normalitas
    values$uji_normal_method <- input$metode_normalitas
    
    if(input$metode_normalitas == "shapiro") {
      if(length(var_data) <= 5000 && length(var_data) >= 3) {
        test_result <- shapiro.test(var_data)
        values$uji_normal_result <- test_result
        
        output$hasil_normalitas <- renderText({
          paste("Shapiro-Wilk Test\n",
                "W =", round(test_result$statistic, 4), "\n",
                "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                "n =", length(var_data), "\n\n",
                "Kesimpulan:",
                if(test_result$p.value < 0.05) {
                  "Tolak H0: Data TIDAK berdistribusi normal (p < 0.05)"
                } else {
                  "Terima H0: Data berdistribusi normal (p >= 0.05)"
                })
        })
        
        output$interpretasi_uji_asumsi <- renderText({
          if(test_result$p.value < 0.05) {
            "Dengan tingkat signifikansi 95%, data tidak memenuhi asumsi sebaran normal. Sebaiknya lakukan transformasi data atau gunakan metode non-parametrik untuk analisis selanjutnya."
          } else {
            "Dengan tingkat signifikansi 95%, data memenuhi asumsi sebaran normal. Analisis dapat dilanjutkan menggunakan metode parametrik."
          }
        })
      } else {
        output$hasil_normalitas <- renderText("Data tidak sesuai untuk Shapiro-Wilk test (n harus antara 3-5000)")
        output$interpretasi_uji_asumsi <- renderText("Silakan pilih metode uji lain atau periksa data Anda.")
      }
    } else if(input$metode_normalitas == "ks") {
      if(length(var_data) > 0) {
        test_result <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
        values$uji_normal_result <- test_result
        
        output$hasil_normalitas <- renderText({
          paste("Kolmogorov-Smirnov Test\n",
                "D =", round(test_result$statistic, 4), "\n",
                "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                "n =", length(var_data), "\n\n",
                "Kesimpulan:",
                if(test_result$p.value < 0.05) {
                  "Tolak H0: Data TIDAK berdistribusi normal (p < 0.05)"
                } else {
                  "Terima H0: Data berdistribusi normal (p >= 0.05)"
                })
        })
        
        output$interpretasi_uji_asumsi <- renderText({
          if (test_result$p.value < 0.05) {
            "Berdasarkan uji Kolmogorov-Smirnov pada tingkat signifikansi 95%, data *tidak memenuhi asumsi normalitas*. 
              Hal ini berarti distribusi data berbeda secara signifikan dari distribusi normal."
          } else {
            "Berdasarkan uji Kolmogorov-Smirnov pada tingkat signifikansi 95%, data *memenuhi asumsi normalitas*. 
              Artinya, penyebaran data tidak berbeda signifikan dari distribusi normal."
          }
        })
      }
    }
  })
  
  # Uji Homogenitas
  observeEvent(input$btn_homogenitas, {
    req(input$var_homogenitas)
    
    var_data <- data_variabel[[input$var_homogenitas]]
    var_data <- var_data[!is.na(var_data)]
    
    # Simpan parameter untuk download
    values$uji_homo_var <- input$var_homogenitas
    values$uji_homo_method <- input$metode_homogenitas
    
    # Bagi data menjadi dua kelompok berdasarkan median
    median_val <- median(var_data)
    group1 <- var_data[var_data <= median_val]
    group2 <- var_data[var_data > median_val]
    
    if(input$metode_homogenitas == "levene") {
      combined_data <- data.frame(
        value = c(group1, group2),
        group = factor(c(rep("Group1", length(group1)), rep("Group2", length(group2))))
      )
      
      if(nrow(combined_data) > 2) {
        test_result <- leveneTest(value ~ group, data = combined_data)
        values$uji_homo_result <- test_result
        
        output$hasil_homogenitas <- renderText({
          paste("Levene Test\n",
                "F =", round(test_result$`F value`[1], 4), "\n",
                "df1 =", test_result$Df[1], "\n",
                "df2 =", test_result$Df[2], "\n",
                "p-value =", format(test_result$`Pr(>F)`[1], scientific = TRUE), "\n",
                "n1 =", length(group1), ", n2 =", length(group2), "\n",
                "Median cutoff =", round(median_val, 3), "\n\n",
                "Kesimpulan:",
                if(test_result$`Pr(>F)`[1] < 0.05) {
                  "Tolak H0: Varians kedua kelompok TIDAK homogen (p < 0.05)"
                } else {
                  "Terima H0: Varians kedua kelompok homogen (p >= 0.05)"
                })
        })
        
        output$interpretasi_uji_asumsi <- renderText({
          if(test_result$`Pr(>F)`[1] < 0.05) {
            "Pada tingkat signifikansi 95%, hasil uji menunjukkan bahwa asumsi homogenitas varians tidak terpenuhi. Artinya, variasi data antar kelompok berbeda secara signifikan. 
            Disarankan untuk mempertimbangkan transformasi data atau menggunakan metode uji yang tidak mensyaratkan varians yang sama (misalnya: uji Welch atau nonparametrik)."
          } else {
            "Pada tingkat signifikansi 95%, asumsi homogenitas varians terpenuhi. Dengan demikian, Anda dapat melanjutkan analisis menggunakan metode uji yang mengasumsikan varians antar kelompok sama."
          }
        })
      }
    } else if(input$metode_homogenitas == "bartlett") {
      combined_data <- data.frame(
        value = c(group1, group2),
        group = factor(c(rep("Group1", length(group1)), rep("Group2", length(group2))))
      )
      
      if(nrow(combined_data) > 2) {
        test_result <- bartlett.test(value ~ group, data = combined_data)
        values$uji_homo_result <- test_result
        
        output$hasil_homogenitas <- renderText({
          paste("Bartlett Test\n",
                "K-squared =", round(test_result$statistic, 4), "\n",
                "df =", test_result$parameter, "\n",
                "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                "n1 =", length(group1), ", n2 =", length(group2), "\n",
                "Median cutoff =", round(median_val, 3), "\n\n",
                "Kesimpulan:",
                if(test_result$p.value < 0.05) {
                  "Tolak H0: Varians kedua kelompok TIDAK homogen (p < 0.05)"
                } else {
                  "Terima H0: Varians kedua kelompok homogen (p >= 0.05)"
                })
        })
        
        output$interpretasi_uji_asumsi <- renderText({
          if(test_result$p.value < 0.05) {
            "Berdasarkan uji Bartlett pada tingkat signifikansi 95%, *asumsi homogenitas varians tidak terpenuhi*. 
              Artinya, terdapat perbedaan signifikan dalam variasi data antar kelompok. Disarankan untuk mempertimbangkan 
            transformasi data atau menggunakan metode alternatif yang tidak mengasumsikan varians yang sama."
          } else {
            "Berdasarkan uji Bartlett pada tingkat signifikansi 95%, asumsi homogenitas varians terpenuhi. 
            Ini berarti variasi data antar kelompok seragam, dan Anda dapat melanjutkan analisis menggunakan uji yang mengasumsikan varians yang sama."
          }
        })
      }
    }
  })
  
  # Uji t Satu Sampel
  observeEvent(input$btn_rata_satu, {
    req(input$var_rata_satu)
    
    var_data <- data_variabel[[input$var_rata_satu]]
    var_data <- var_data[!is.na(var_data)]
    
    # Simpan parameter untuk download
    values$rata_var <- input$var_rata_satu
    values$rata_method <- "t_one"
    values$rata_hypothesis <- input$nilai_hipotesis_rata
    values$rata_alternative <- input$alternative_rata_satu
    
    if(length(var_data) > 1) {
      test_result <- t.test(var_data, mu = input$nilai_hipotesis_rata, alternative = input$alternative_rata_satu)
      values$rata_result <- test_result
      
      output$hasil_rata_satu <- renderText({
        paste("Uji t Satu Sampel\n",
              "t =", round(test_result$statistic, 4), "\n",
              "df =", test_result$parameter, "\n",
              "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
              "95% CI: [", round(test_result$conf.int[1], 4), ",", round(test_result$conf.int[2], 4), "]\n",
              "Sample mean =", round(mean(var_data), 4), "\n",
              "Hypothesized mean =", input$nilai_hipotesis_rata, "\n",
              "n =", length(var_data), "\n\n",
              "Kesimpulan:",
              if(test_result$p.value < 0.05) {
                paste("Tolak H0: Rata-rata populasi BERBEDA SIGNIFIKAN dari", input$nilai_hipotesis_rata, "(p < 0.05)")
              } else {
                paste("Terima H0: Rata-rata populasi TIDAK berbeda signifikan dari", input$nilai_hipotesis_rata, "(p >= 0.05)")
              })
      })
      
      output$interpretasi_rata <- renderText({
        effect_size <- abs(mean(var_data) - input$nilai_hipotesis_rata) / sd(var_data)
        if(test_result$p.value < 0.05) {
          paste("Hasil uji menunjukkan bahwa terdapat perbedaan yang signifikan antara rata-rata sampel dan nilai hipotesis pada tingkat signifikansi 95%.",
                "Ini berarti nilai rata-rata dalam data kemungkinan berbeda nyata dari nilai yang diasumsikan.",
                "\n Effect size (Cohen's d) =", round(effect_size, 3),
                ifelse(effect_size < 0.2, "(efek sangat kecil)", 
                       ifelse(effect_size < 0.8, "(efek sedang)", "(efek besar)")),
                "memberikan gambaran tentang seberapa besar perbedaan yang ditemukan.")
        } else {
          paste("Hasil uji menunjukkan tidak terdapat perbedaan signifikan antara rata-rata sampel dan nilai hipotesis pada tingkat signifikansi 95%.",
                "Dengan kata lain, data mendukung hipotesis nol bahwa rata-rata populasi sama dengan nilai yang diuji.",
                "\n Effect size (Cohen's d) =", round(effect_size, 3),
                ifelse(effect_size < 0.2, "(efek sangat kecil)", ifelse(effect_size < 0.8, "(efek sedang)", "(efek besar)")),
                "menunjukkan bahwa meskipun ada perbedaan numerik, ukuran efeknya kecil atau tidak cukup kuat secara statistik."
          )
        }
      })
    }
  })
  
  # Uji t Dua Kelompok
  observeEvent(input$btn_rata_dua, {
    req(input$var_rata_dua1, input$var_rata_dua2)
    
    var1_data <- data_variabel[[input$var_rata_dua1]]
    var2_data <- data_variabel[[input$var_rata_dua2]]
    
    var1_data <- var1_data[!is.na(var1_data)]
    var2_data <- var2_data[!is.na(var2_data)]
    
    # Simpan parameter untuk download
    values$rata_var <- input$var_rata_dua1
    values$rata_var2 <- input$var_rata_dua2
    values$rata_method <- input$metode_rata_dua
    values$rata_var_equal <- input$var_equal
    
    if(input$metode_rata_dua == "t_two") {
      if(length(var1_data) > 1 && length(var2_data) > 1) {
        test_result <- t.test(var1_data, var2_data, var.equal = input$var_equal)
        values$rata_result <- test_result
        
        output$hasil_rata_dua <- renderText({
          paste("Uji t Dua Sampel\n",
                "t =", round(test_result$statistic, 4), "\n",
                "df =", round(test_result$parameter, 2), "\n",
                "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                "95% CI: [", round(test_result$conf.int[1], 4), ",", round(test_result$conf.int[2], 4), "]\n",
                "Mean 1 =", round(mean(var1_data), 4), "\n",
                "Mean 2 =", round(mean(var2_data), 4), "\n",
                "Mean difference =", round(mean(var1_data) - mean(var2_data), 4), "\n",
                "n1 =", length(var1_data), ", n2 =", length(var2_data), "\n",
                "Equal variances assumed:", input$var_equal, "\n\n",
                "Kesimpulan:",
                if(test_result$p.value < 0.05) {
                  "Tolak H0: Terdapat PERBEDAAN SIGNIFIKAN antara kedua variabel (p < 0.05)"
                } else {
                  "Terima H0: TIDAK terdapat perbedaan signifikan antara kedua variabel (p >= 0.05)"
                })
        })
        
        output$interpretasi_rata <- renderText({
          pooled_sd <- sqrt(((length(var1_data)-1)*var(var1_data) + (length(var2_data)-1)*var(var2_data)) /
                              (length(var1_data) + length(var2_data) - 2))
          effect_size <- abs(mean(var1_data) - mean(var2_data)) / pooled_sd
          
          if (test_result$p.value < 0.05) {
            paste(
              "Hasil analisis menunjukkan adanya perbedaan yang signifikan antara kedua kelompok pada tingkat signifikansi 95%.",
              "Nilai effect size (Cohen's d) =", round(effect_size, 3),
              ifelse(effect_size < 0.2, "(kategori kecil)", 
                     ifelse(effect_size < 0.8, "(kategori sedang)", "(kategori besar)")),
              "yang menggambarkan seberapa besar perbedaan rata-rata antar kelompok secara praktis."
            )
          } else {
            paste(
              "Tidak ditemukan perbedaan yang signifikan antara kedua kelompok berdasarkan hasil uji pada tingkat signifikansi 95%.",
              "Secara statistik, rata-rata kedua kelompok dapat dianggap sama.",
              "Effect size (Cohen's d) =", round(effect_size, 3),
              ifelse(effect_size < 0.2, "(kategori kecil)", 
                     ifelse(effect_size < 0.8, "(kategori sedang)", "(kategori besar)")),
              "menunjukkan bahwa perbedaan antar kelompok tergolong kecil atau kurang bermakna secara praktis."
            )
          }
        })
      }
    } else if(input$metode_rata_dua == "t_paired") {
      min_length <- min(length(var1_data), length(var2_data))
      var1_clean <- var1_data[1:min_length]
      var2_clean <- var2_data[1:min_length]
      
      if(length(var1_clean) > 1) {
        test_result <- t.test(var1_clean, var2_clean, paired = TRUE)
        values$rata_result <- test_result
        
        output$hasil_rata_dua <- renderText({
          paste("Uji t Berpasangan\n",
                "t =", round(test_result$statistic, 4), "\n",
                "df =", test_result$parameter, "\n",
                "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                "95% CI: [", round(test_result$conf.int[1], 4), ",", round(test_result$conf.int[2], 4), "]\n",
                "Mean difference =", round(mean(var1_clean - var2_clean), 4), "\n",
                "n pairs =", length(var1_clean), "\n\n",
                "Kesimpulan:",
                if(test_result$p.value < 0.05) {
                  "Tolak H0: Terdapat PERBEDAAN SIGNIFIKAN antara kedua pengukuran (p < 0.05)"
                } else {
                  "Terima H0: TIDAK terdapat perbedaan signifikan antara kedua pengukuran (p >= 0.05)"
                })
        })
        
        output$interpretasi_rata <- renderText({
          diff_data <- var1_clean - var2_clean
          effect_size <- abs(mean(diff_data)) / sd(diff_data)
          
          if (test_result$p.value < 0.05) {
            paste(
              "Hasil uji menunjukkan adanya perbedaan yang signifikan antara kedua pengukuran berpasangan pada tingkat signifikansi 95%.",
              "Effect size =", round(effect_size, 3),
              "menggambarkan besarnya perbedaan secara praktis antar dua kondisi yang diuji."
            )
          } else {
            paste(
              "Tidak ditemukan perbedaan yang signifikan antara kedua pengukuran berpasangan berdasarkan hasil uji pada tingkat signifikansi 95%.",
              "Secara statistik, kedua kondisi dapat dianggap memiliki hasil yang setara."
            )
          }
        })
      }
    }
  })
  
  # Uji Proporsi Satu Kelompok
  observeEvent(input$btn_proporsi_satu, {
    req(input$var_proporsi_satu)
    
    var_data <- data_variabel[[input$var_proporsi_satu]]
    var_data <- var_data[!is.na(var_data)]
    
    success_count <- sum(var_data > input$nilai_batas_satu)
    total_count <- length(var_data)
    
    # Simpan parameter untuk download
    values$proporsi_var <- input$var_proporsi_satu
    values$proporsi_batas <- input$nilai_batas_satu
    values$proporsi_hypothesis <- input$proporsi_hipotesis_satu
    values$proporsi_success <- success_count
    values$proporsi_total <- total_count
    values$proporsi_alternative <- input$alternative_proporsi_satu
    
    if(total_count > 0 && success_count > 0 && success_count < total_count) {
      test_result <- prop.test(success_count, total_count, p = input$proporsi_hipotesis_satu,
                               alternative = input$alternative_proporsi_satu)
      values$proporsi_result <- test_result
      
      output$hasil_proporsi_satu <- renderText({
        observed_prop <- success_count/total_count
        paste("Uji Proporsi Satu Kelompok\n",
              "Jumlah sukses:", success_count, "\n",
              "Total observasi:", total_count, "\n",
              "Proporsi sampel:", round(observed_prop, 4), "\n",
              "Proporsi hipotesis:", input$proporsi_hipotesis_satu, "\n",
              "X-squared =", round(test_result$statistic, 4), "\n",
              "df =", test_result$parameter, "\n",
              "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
              "95% CI: [", round(test_result$conf.int[1], 4), ",", round(test_result$conf.int[2], 4), "]\n\n",
              "Kesimpulan:",
              if(test_result$p.value < 0.05) {
                paste("Tolak H0: Proporsi populasi BERBEDA SIGNIFIKAN dari", input$proporsi_hipotesis_satu, "(p < 0.05)")
              } else {
                paste("Terima H0: Proporsi populasi TIDAK berbeda signifikan dari", input$proporsi_hipotesis_satu, "(p >= 0.05)")
              })
      })
      
      output$interpretasi_proporsi <- renderText({
        observed_prop <- success_count / total_count
        diff_prop <- abs(observed_prop - input$proporsi_hipotesis_satu)
        
        if (test_result$p.value < 0.05) {
          paste(
            "Proporsi yang diamati (", round(observed_prop, 4), 
            ") berbeda secara signifikan dari proporsi yang dihipotesiskan (", 
            input$proporsi_hipotesis_satu, ") pada tingkat signifikansi 95%.", 
            "Perbedaan proporsi =", round(diff_prop, 4)
          )
        } else {
          paste(
            "Tidak ditemukan perbedaan signifikan antara proporsi yang diamati dan proporsi yang dihipotesiskan.", 
            "Data mendukung hipotesis nol pada tingkat signifikansi 95%."
          )
        }
      })
    } else {
      output$hasil_proporsi_satu <- renderText("Data tidak memenuhi syarat untuk uji proporsi (perlu ada variasi dalam data)")
      output$interpretasi_proporsi <- renderText("Periksa nilai batas kategorisasi atau data yang digunakan.")
    }
  })
  
  # Uji Proporsi Dua Kelompok
  observeEvent(input$btn_proporsi_dua, {
    req(input$var_proporsi_dua1, input$var_proporsi_dua2)
    
    var1_data <- data_variabel[[input$var_proporsi_dua1]]
    var2_data <- data_variabel[[input$var_proporsi_dua2]]
    
    var1_data <- var1_data[!is.na(var1_data)]
    var2_data <- var2_data[!is.na(var2_data)]
    
    success1 <- sum(var1_data > input$nilai_batas_dua)
    success2 <- sum(var2_data > input$nilai_batas_dua)
    total1 <- length(var1_data)
    total2 <- length(var2_data)
    
    if(total1 > 0 && total2 > 0 && success1 > 0 && success2 > 0) {
      test_result <- prop.test(c(success1, success2), c(total1, total2))
      values$proporsi_result <- test_result
      
      output$hasil_proporsi_dua <- renderText({
        prop1 <- success1/total1
        prop2 <- success2/total2
        paste("Uji Proporsi Dua Kelompok\n",
              "Kelompok 1 - Sukses:", success1, "Total:", total1, "Proporsi:", round(prop1, 4), "\n",
              "Kelompok 2 - Sukses:", success2, "Total:", total2, "Proporsi:", round(prop2, 4), "\n",
              "Selisih proporsi:", round(prop1 - prop2, 4), "\n",
              "X-squared =", round(test_result$statistic, 4), "\n",
              "df =", test_result$parameter, "\n",
              "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
              "95% CI: [", round(test_result$conf.int[1], 4), ",", round(test_result$conf.int[2], 4), "]\n\n",
              "Kesimpulan:",
              if(test_result$p.value < 0.05) {
                "Tolak H0: Terdapat PERBEDAAN SIGNIFIKAN proporsi antara kedua kelompok (p < 0.05)"
              } else {
                "Terima H0: TIDAK terdapat perbedaan signifikan proporsi antara kedua kelompok (p >= 0.05)"
              })
      })
      
      output$interpretasi_proporsi <- renderText({
        prop1 <- success1/total1
        prop2 <- success2/total2
        
        if (test_result$p.value < 0.05) {
          paste(
            "Terdapat perbedaan proporsi yang signifikan antara kedua kelompok pada tingkat signifikansi 95%.",
            "Kelompok 1 memiliki proporsi sebesar", round(prop1, 4), 
            "sementara kelompok 2 memiliki proporsi sebesar", round(prop2, 4), 
            "menunjukkan adanya perbedaan nyata secara statistik."
          )
        } else {
          paste(
            "Tidak ditemukan perbedaan proporsi yang signifikan antara kedua kelompok.",
            "Proporsi kelompok 1 (", round(prop1, 4), 
            ") dan kelompok 2 (", round(prop2, 4), 
            ") secara statistik dapat dianggap sama pada tingkat signifikansi 95%."
          )
        }
      })
    } else {
      output$hasil_proporsi_dua <- renderText(
        "Uji proporsi dua kelompok tidak dapat dilakukan karena data tidak memenuhi syarat yang diperlukan."
      )
      output$interpretasi_proporsi <- renderText(
        "Silakan periksa kembali nilai batas kategorisasi atau integritas data yang digunakan agar uji dapat dijalankan dengan valid."
      )
    }
  })
  
  # Uji Variance Satu Kelompok (Chi-square test untuk variance)
  observeEvent(input$btn_variance_satu, {
    req(input$var_variance_satu)
    
    var_data <- data_variabel[[input$var_variance_satu]]
    var_data <- var_data[!is.na(var_data)]
    
    # Simpan parameter untuk download
    values$variance_var1 <- input$var_variance_satu
    values$variance_method <- "chi_square"
    values$variance_hypothesis <- input$variance_hipotesis
    
    if(length(var_data) > 1) {
      n <- length(var_data)
      sample_var <- var(var_data)
      chi_stat <- (n - 1) * sample_var / input$variance_hipotesis
      p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
      
      values$variance_result <- list(
        statistic = chi_stat,
        parameter = n-1,
        p.value = p_value,
        sample_var = sample_var,
        hypothesis_var = input$variance_hipotesis
      )
      
      output$hasil_variance_satu <- renderText({
        paste("Uji Variance Satu Kelompok (Chi-square Test)\n",
              "Sample variance =", round(sample_var, 4), "\n",
              "Hypothesized variance =", input$variance_hipotesis, "\n",
              "Chi-squared =", round(chi_stat, 4), "\n",
              "df =", n-1, "\n",
              "p-value =", format(p_value, scientific = TRUE), "\n",
              "n =", n, "\n\n",
              "Kesimpulan:",
              if(p_value < 0.05) {
                paste("Tolak H0: Variance populasi BERBEDA SIGNIFIKAN dari", input$variance_hipotesis, "(p < 0.05)")
              } else {
                paste("Terima H0: Variance populasi TIDAK berbeda signifikan dari", input$variance_hipotesis, "(p >= 0.05)")
              })
      })
      
      output$interpretasi_variance <- renderText({
        ratio <- sample_var / input$variance_hipotesis
        if(p_value < 0.05) {
          paste(
            "Varians sampel (", round(sample_var, 4), 
            ") berbeda secara signifikan dari varians yang dihipotesiskan (", 
            input$variance_hipotesis, ") pada tingkat signifikansi 95%.", 
            "Rasio varians =", round(ratio, 3), 
            ". Hasil ini menunjukkan adanya ketidaksesuaian dengan asumsi awal."
          )
        } else {
          paste(
            "Tidak ditemukan perbedaan signifikan antara varians sampel dan varians yang dihipotesiskan pada tingkat signifikansi 95%.", 
            "Rasio varians =", round(ratio, 3), 
            ". Hasil ini mendukung hipotesis nol bahwa varians tidak berbeda secara statistik."
          )
        }
      })
    }
  })
  
  # Uji Variance Dua Kelompok
  observeEvent(input$btn_variance_dua, {
    req(input$var_variance_dua1, input$var_variance_dua2)
    
    var1_data <- data_variabel[[input$var_variance_dua1]]
    var2_data <- data_variabel[[input$var_variance_dua2]]
    
    var1_data <- var1_data[!is.na(var1_data)]
    var2_data <- var2_data[!is.na(var2_data)]
    
    # Simpan parameter untuk download
    values$variance_var1 <- input$var_variance_dua1
    values$variance_var2 <- input$var_variance_dua2
    values$variance_method <- input$metode_variance_dua
    
    if(input$metode_variance_dua == "ftest") {
      if(length(var1_data) > 1 && length(var2_data) > 1) {
        test_result <- var.test(var1_data, var2_data)
        values$variance_result <- test_result
        
        output$hasil_variance_dua <- renderText({
          paste("F Test untuk Variance\n",
                "F =", round(test_result$statistic, 4), "\n",
                "df1 =", test_result$parameter[1], "\n",
                "df2 =", test_result$parameter[2], "\n",
                "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                "Variance 1 =", round(var(var1_data), 4), "\n",
                "Variance 2 =", round(var(var2_data), 4), "\n",
                "Variance ratio =", round(var(var1_data)/var(var2_data), 4), "\n",
                "95% CI: [", round(test_result$conf.int[1], 4), ",", round(test_result$conf.int[2], 4), "]\n\n",
                "Kesimpulan:",
                if(test_result$p.value < 0.05) {
                  "Tolak H0: Varians kedua kelompok BERBEDA SIGNIFIKAN (p < 0.05)"
                } else {
                  "Terima H0: Varians kedua kelompok TIDAK berbeda signifikan (p >= 0.05)"
                })
        })
        
        output$interpretasi_variance <- renderText({
          var1 <- var(var1_data)
          var2 <- var(var2_data)
          ratio <- var1 / var2
          
          if(test_result$p.value < 0.05) {
            paste(
              "Terdapat perbedaan signifikan antara varians kedua kelompok pada tingkat signifikansi 95%.",
              "Rasio varians =", round(ratio, 3),
              ifelse(ratio > 1, 
                     "(Kelompok 1 memiliki variasi yang lebih besar)", 
                     "(Kelompok 2 memiliki variasi yang lebih besar)"),
              "Hal ini menunjukkan bahwa asumsi homogenitas varians tidak terpenuhi."
            )
          } else {
            paste(
              "Tidak terdapat perbedaan signifikan antara varians kedua kelompok pada tingkat signifikansi 95%.",
              "Rasio varians =", round(ratio, 3),
              "Hasil ini menunjukkan bahwa asumsi homogenitas varians terpenuhi."
            )
          }
        })
      }
    } else if(input$metode_variance_dua == "levene") {
      combined_data <- data.frame(
        value = c(var1_data, var2_data),
        group = factor(c(rep("Group1", length(var1_data)), rep("Group2", length(var2_data))))
      )
      
      if(nrow(combined_data) > 2) {
        test_result <- leveneTest(value ~ group, data = combined_data)
        values$variance_result <- test_result
        
        output$hasil_variance_dua <- renderText({
          paste("Levene Test untuk Variance\n",
                "F =", round(test_result$`F value`[1], 4), "\n",
                "df1 =", test_result$Df[1], "\n",
                "df2 =", test_result$Df[2], "\n",
                "p-value =", format(test_result$`Pr(>F)`[1], scientific = TRUE), "\n",
                "n1 =", length(var1_data), ", n2 =", length(var2_data), "\n\n",
                "Kesimpulan:",
                if(test_result$`Pr(>F)`[1] < 0.05) {
                  "Tolak H0: Varians kedua kelompok BERBEDA SIGNIFIKAN (p < 0.05)"
                } else {
                  "Terima H0: Varians kedua kelompok TIDAK berbeda signifikan (p >= 0.05)"
                })
        })
        
        output$interpretasi_variance <- renderText({
          if (test_result$`Pr(>F)`[1] < 0.05) {
            "Hasil uji Levene menunjukkan adanya perbedaan signifikan antara varians kedua kelompok. Dengan demikian, asumsi homogenitas varians tidak terpenuhi, dan analisis lanjutan perlu mempertimbangkan metode yang robust terhadap perbedaan varians."
          } else {
            "Hasil uji Levene menunjukkan tidak ada perbedaan signifikan antara varians kedua kelompok. Asumsi homogenitas varians terpenuhi sehingga analisis dapat dilanjutkan dengan metode yang mengasumsikan varians homogen."
          }
        })
      }
    }
  })
  
  # ANOVA Satu Arah
  observeEvent(input$btn_anova_satu, {
    req(input$var_anova_satu_y, input$var_anova_satu_group)
    
    y_data <- data_variabel[[input$var_anova_satu_y]]
    group_data <- data_variabel[[input$var_anova_satu_group]]
    
    complete_cases <- complete.cases(y_data, group_data)
    y_clean <- y_data[complete_cases]
    group_clean <- group_data[complete_cases]
    
    n_groups <- input$n_groups_satu
    breaks <- quantile(group_clean, probs = seq(0, 1, length.out = n_groups + 1), na.rm = TRUE)
    group_categories <- cut(group_clean, breaks = breaks, labels = paste("Group", 1:n_groups), include.lowest = TRUE)
    
    anova_data <- data.frame(
      y = y_clean,
      group = group_categories
    )
    
    anova_data <- anova_data[!is.na(anova_data$group), ]
    
    # Simpan parameter untuk download
    values$anova_var_y <- input$var_anova_satu_y
    values$anova_var_group <- input$var_anova_satu_group
    values$anova_n_groups <- input$n_groups_satu
    values$anova_data <- anova_data
    values$anova_type <- "one_way"
    
    if(nrow(anova_data) > n_groups) {
      anova_result <- aov(y ~ group, data = anova_data)
      anova_summary <- summary(anova_result)
      values$anova_result <- anova_summary
      
      # Hitung group means
      group_means <- aggregate(y ~ group, data = anova_data, FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
      
      output$hasil_anova_satu <- renderText({
        paste("ANOVA Satu Arah\n",
              "F =", round(anova_summary[[1]]$`F value`[1], 4), "\n",
              "df1 =", anova_summary[[1]]$Df[1], "\n",
              "df2 =", anova_summary[[1]]$Df[2], "\n",
              "p-value =", format(anova_summary[[1]]$`Pr(>F)`[1], scientific = TRUE), "\n",
              "MSE =", round(anova_summary[[1]]$`Mean Sq`[2], 4), "\n",
              "R-squared =", round(anova_summary[[1]]$`Sum Sq`[1] / sum(anova_summary[[1]]$`Sum Sq`), 4), "\n\n",
              "Group Statistics:\n",
              paste(capture.output({
                for(i in 1:nrow(group_means)) {
                  cat(paste0(group_means$group[i], ": Mean = ", round(group_means$y[i,1], 3),
                             ", SD = ", round(group_means$y[i,2], 3),
                             ", n = ", group_means$y[i,3], "\n"))
                }
              }), collapse = ""), "\n",
              "Kesimpulan:",
              if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
                "Tolak H0: Terdapat PERBEDAAN SIGNIFIKAN antar kelompok (p < 0.05)"
              } else {
                "Terima H0: TIDAK terdapat perbedaan signifikan antar kelompok (p >= 0.05)"
              })
      })
      
      output$interpretasi_anova <- renderText({
        eta_squared <- anova_summary[[1]]$`Sum Sq`[1] / sum(anova_summary[[1]]$`Sum Sq`)
        
        if (anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
          paste(
            "Hasil uji ANOVA menunjukkan adanya perbedaan rata-rata yang signifikan antar kelompok dalam variabel",
            input$var_anova_satu_y, "pada tingkat signifikansi 95%.",
            "Nilai eta-squared =", round(eta_squared, 4),
            ifelse(eta_squared < 0.01, "(efek kecil)", 
                   ifelse(eta_squared < 0.06, "(efek sedang)", "(efek besar)")),
            "Diperlukan uji post-hoc untuk mengidentifikasi kelompok yang berbeda secara signifikan."
          )
        } else {
          paste(
            "Hasil uji ANOVA menunjukkan tidak adanya perbedaan rata-rata yang signifikan antar kelompok dalam variabel",
            input$var_anova_satu_y, "pada tingkat signifikansi 95%.",
            "Hal ini mengindikasikan bahwa rata-rata antar kelompok tidak berbeda secara statistik."
          )
        }
      })
    } else {
      output$hasil_anova_satu <- renderText("Data tidak cukup untuk melakukan ANOVA. Periksa data dan pengaturan kelompok.")
      output$interpretasi_anova <- renderText("Silakan pilih variabel lain atau ubah jumlah kelompok.")
    }
  })
  
  # ANOVA Dua Arah
  observeEvent(input$btn_anova_dua, {
    req(input$var_anova_dua_y, input$var_anova_dua_group1, input$var_anova_dua_group2)
    
    y_data <- data_variabel[[input$var_anova_dua_y]]
    group1_data <- data_variabel[[input$var_anova_dua_group1]]
    group2_data <- data_variabel[[input$var_anova_dua_group2]]
    
    complete_cases <- complete.cases(y_data, group1_data, group2_data)
    y_clean <- y_data[complete_cases]
    group1_clean <- group1_data[complete_cases]
    group2_clean <- group2_data[complete_cases]
    
    # Kategorisasi faktor 1
    n_groups1 <- input$n_groups_dua1
    breaks1 <- quantile(group1_clean, probs = seq(0, 1, length.out = n_groups1 + 1), na.rm = TRUE)
    group1_categories <- cut(group1_clean, breaks = breaks1, labels = paste("F1_", 1:n_groups1), include.lowest = TRUE)
    
    # Kategorisasi faktor 2
    n_groups2 <- input$n_groups_dua2
    breaks2 <- quantile(group2_clean, probs = seq(0, 1, length.out = n_groups2 + 1), na.rm = TRUE)
    group2_categories <- cut(group2_clean, breaks = breaks2, labels = paste("F2_", 1:n_groups2), include.lowest = TRUE)
    
    anova_data <- data.frame(
      y = y_clean,
      factor1 = group1_categories,
      factor2 = group2_categories
    )
    
    anova_data <- anova_data[complete.cases(anova_data), ]
    
    # Simpan parameter untuk download
    values$anova_var_y <- input$var_anova_dua_y
    values$anova_var_group1 <- input$var_anova_dua_group1
    values$anova_var_group2 <- input$var_anova_dua_group2
    values$anova_n_groups1 <- input$n_groups_dua1
    values$anova_n_groups2 <- input$n_groups_dua2
    values$anova_data <- anova_data
    values$anova_type <- "two_way"
    
    if(nrow(anova_data) > (n_groups1 * n_groups2)) {
      anova_result <- aov(y ~ factor1 * factor2, data = anova_data)
      anova_summary <- summary(anova_result)
      values$anova_result <- anova_summary
      
      output$hasil_anova_dua <- renderText({
        paste("ANOVA Dua Arah\n",
              "Faktor 1 (", input$var_anova_dua_group1, "):\n",
              "  F =", round(anova_summary[[1]]$`F value`[1], 4), 
              ", p-value =", format(anova_summary[[1]]$`Pr(>F)`[1], scientific = TRUE), "\n",
              "Faktor 2 (", input$var_anova_dua_group2, "):\n",
              "  F =", round(anova_summary[[1]]$`F value`[2], 4), 
              ", p-value =", format(anova_summary[[1]]$`Pr(>F)`[2], scientific = TRUE), "\n",
              "Interaksi (", input$var_anova_dua_group1, " × ", input$var_anova_dua_group2, "):\n",
              "  F =", round(anova_summary[[1]]$`F value`[3], 4), 
              ", p-value =", format(anova_summary[[1]]$`Pr(>F)`[3], scientific = TRUE), "\n\n",
              "df Error =", anova_summary[[1]]$Df[4], "\n",
              "MSE =", round(anova_summary[[1]]$`Mean Sq`[4], 4), "\n\n",
              "Kesimpulan:\n",
              "Faktor 1:", if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) "Signifikan" else "Tidak signifikan", "\n",
              "Faktor 2:", if(anova_summary[[1]]$`Pr(>F)`[2] < 0.05) "Signifikan" else "Tidak signifikan", "\n",
              "Interaksi:", if(anova_summary[[1]]$`Pr(>F)`[3] < 0.05) "Signifikan" else "Tidak signifikan")
      })
      
      output$interpretasi_anova <- renderText({
        total_ss <- sum(anova_summary[[1]]$`Sum Sq`)
        eta1 <- anova_summary[[1]]$`Sum Sq`[1] / total_ss
        eta2 <- anova_summary[[1]]$`Sum Sq`[2] / total_ss
        eta_int <- anova_summary[[1]]$`Sum Sq`[3] / total_ss
        
        interpretation <- paste0("Hasil ANOVA Dua Arah untuk variabel ", input$var_anova_dua_y, ":\n\n")
        
        if (anova_summary[[1]]$`Pr(>F)`[3] < 0.05) {
          interpretation <- paste0(
            interpretation,
            "Terdapat **efek interaksi yang signifikan** antara Faktor 1 dan Faktor 2 pada tingkat signifikansi 95% (p < 0.05).\n",
            "Hal ini menunjukkan bahwa pengaruh salah satu faktor bergantung pada tingkat dari faktor lainnya.\n\n"
          )
        } else {
          interpretation <- paste0(
            interpretation,
            "Tidak terdapat efek interaksi yang signifikan antara Faktor 1 dan Faktor 2 pada tingkat signifikansi 95% (p ≥ 0.05).\n"
          )
          
          if (anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
            interpretation <- paste0(
              interpretation,
              "- Faktor 1 memiliki **efek utama yang signifikan** terhadap variabel pada tingkat signifikansi 95%.\n"
            )
          } else {
            interpretation <- paste0(
              interpretation,
              "- Faktor 1 **tidak memiliki efek utama yang signifikan** terhadap variabel.\n"
            )
          }
          
          if (anova_summary[[1]]$`Pr(>F)`[2] < 0.05) {
            interpretation <- paste0(
              interpretation,
              "- Faktor 2 memiliki **efek utama yang signifikan** terhadap variabel pada tingkat signifikansi 95%.\n"
            )
          } else {
            interpretation <- paste0(
              interpretation,
              "- Faktor 2 **tidak memiliki efek utama yang signifikan** terhadap variabel.\n"
            )
          }
          
          interpretation <- paste0(interpretation, "\n")
        }
        
        paste0(
          interpretation,
          "Besaran efek (η²):\n",
          "- Faktor 1: ", round(eta1, 4), "\n",
          "- Faktor 2: ", round(eta2, 4), "\n",
          "- Interaksi: ", round(eta_int, 4)
        )
      })
    } else {
      output$hasil_anova_dua <- renderText("Data tidak cukup untuk melakukan ANOVA dua arah. Periksa data dan pengaturan kelompok.")
      output$interpretasi_anova <- renderText("Silakan pilih variabel lain atau ubah jumlah level faktor.")
    }
  })
  
  # Regresi Linear
  observeEvent(input$btn_regresi, {
    req(input$var_regresi_y, input$var_regresi_x)
    
    if(input$include_intercept) {
      formula_str <- paste(input$var_regresi_y, "~", paste(input$var_regresi_x, collapse = " + "))
    } else {
      formula_str <- paste(input$var_regresi_y, "~ -1 +", paste(input$var_regresi_x, collapse = " + "))
    }
    formula_obj <- as.formula(formula_str)
    
    # Simpan parameter untuk download
    values$regresi_var_y <- input$var_regresi_y
    values$regresi_var_x <- input$var_regresi_x
    values$regresi_formula <- formula_str
    values$regresi_include_intercept <- input$include_intercept
    
    tryCatch({
      model <- lm(formula_obj, data = data_variabel)
      values$regresi_model <- model
      
      # Hitung additional statistics
      model_summary <- summary(model)
      
      output$hasil_regresi <- renderText({
        paste("REGRESI LINEAR BERGANDA\n",
              "Formula:", formula_str, "\n\n",
              "Coefficients:\n",
              paste(capture.output(print(model_summary$coefficients)), collapse = "\n"), "\n\n",
              "Model Statistics:\n",
              "R-squared:", round(model_summary$r.squared, 4), "\n",
              "Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n",
              "F-statistic:", round(model_summary$fstatistic[1], 4), "\n",
              "p-value:", format(pf(model_summary$fstatistic[1],
                                    model_summary$fstatistic[2],
                                    model_summary$fstatistic[3],
                                    lower.tail = FALSE), scientific = TRUE), "\n",
              "Residual standard error:", round(model_summary$sigma, 4), "\n",
              "Degrees of freedom:", model_summary$df[2], "\n\n",
              "ANOVA Table:\n",
              paste(capture.output(print(anova(model))), collapse = "\n"))
      })
      
      output$plot_diagnostik <- renderPlot({
        par(mfrow = c(2, 2))
        plot(model, which = 1:4)
      })
      
      output$interpretasi_regresi <- renderText({
        r_squared <- model_summary$r.squared
        adj_r_squared <- model_summary$adj.r.squared
        f_stat <- model_summary$fstatistic[1]
        p_value <- pf(f_stat, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
        
        # Ambil p-value dari masing-masing koefisien
        coef_p_values <- model_summary$coefficients[, 4]
        
        # Tentukan variabel signifikan (selain intercept jika dipilih)
        if (input$include_intercept) {
          significant_vars <- names(coef_p_values)[-1][coef_p_values[-1] < 0.05]
        } else {
          significant_vars <- names(coef_p_values)[coef_p_values < 0.05]
        }
        
        # Interpretasi awal R² dan Adjusted R²
        interpretation <- paste0(
          "Model regresi menjelaskan sebesar ", round(r_squared * 100, 2), "% variasi pada variabel ",
          input$var_regresi_y, ".\n",
          "Nilai R² = ", round(r_squared, 4),
          ", dan Adjusted R² = ", round(adj_r_squared, 4), ".\n\n"
        )
        
        # Uji signifikansi keseluruhan model
        if (p_value < 0.05) {
          interpretation <- paste0(interpretation, "Secara keseluruhan, model regresi signifikan (p < 0.05).\n")
        } else {
          interpretation <- paste0(interpretation, "Secara keseluruhan, model regresi **tidak signifikan** (p ≥ 0.05).\n")
        }
        
        # Daftar variabel signifikan
        if (length(significant_vars) > 0) {
          interpretation <- paste0(
            interpretation,
            "Variabel independen yang berpengaruh signifikan terhadap ", input$var_regresi_y, ": ",
            paste(significant_vars, collapse = ", "), ".\n"
          )
        } else {
          interpretation <- paste0(interpretation, "Tidak ada variabel independen yang signifikan.\n")
        }
        
        # Catatan akhir
        interpretation <- paste0(interpretation, "\nDisarankan untuk memeriksa plot diagnostik untuk memastikan asumsi model terpenuhi.")
        
        interpretation
      })
    }, error = function(e) {
      output$hasil_regresi <- renderText(paste("Error dalam analisis regresi:", e$message,
                                               "\nPeriksa variabel yang dipilih dan pastikan tidak ada multikolinearitas sempurna."))
      output$interpretasi_regresi <- renderText("Silakan pilih kombinasi variabel yang berbeda atau periksa data untuk missing values.")
    })
  })
  
  # UJI ASUMSI REGRESI
  observeEvent(input$btn_uji_asumsi_regresi, {
    req(values$regresi_model)
    
    model <- values$regresi_model
    
    # 1. Uji Normalitas Residual (Shapiro-Wilk)
    residuals_data <- residuals(model)
    
    if(length(residuals_data) <= 5000 && length(residuals_data) >= 3) {
      normalitas_test <- shapiro.test(residuals_data)
      
      output$hasil_normalitas_residual <- renderText({
        paste("Shapiro-Wilk Test untuk Normalitas Residual\n",
              "W =", round(normalitas_test$statistic, 4), "\n",
              "p-value =", format(normalitas_test$p.value, scientific = TRUE), "\n",
              "n =", length(residuals_data), "\n\n",
              "Kesimpulan:",
              if(normalitas_test$p.value < 0.05) {
                "PELANGGARAN: Residual TIDAK berdistribusi normal (p < 0.05)"
              } else {
                "TERPENUHI: Residual berdistribusi normal (p >= 0.05)"
              })
      })
      
      values$normalitas_residual <- normalitas_test
    } else {
      output$hasil_normalitas_residual <- renderText("Jumlah observasi tidak sesuai untuk Shapiro-Wilk test")
    }
    
    # 2. Uji Homoskedastisitas (Breusch-Pagan)
    tryCatch({
      bp_test <- bptest(model)
      
      output$hasil_homoskedastisitas <- renderText({
        paste("Breusch-Pagan Test untuk Homoskedastisitas\n",
              "BP =", round(bp_test$statistic, 4), "\n",
              "df =", bp_test$parameter, "\n",
              "p-value =", format(bp_test$p.value, scientific = TRUE), "\n\n",
              "Kesimpulan:",
              if(bp_test$p.value < 0.05) {
                "PELANGGARAN: Terdapat heteroskedastisitas (p < 0.05)"
              } else {
                "TERPENUHI: Homoskedastisitas terpenuhi (p >= 0.05)"
              })
      })
      
      values$homoskedastisitas <- bp_test
    }, error = function(e) {
      output$hasil_homoskedastisitas <- renderText("Error dalam uji Breusch-Pagan")
    })
    
    # 3. Uji Non-Autokorelasi (Durbin-Watson)
    tryCatch({
      dw_test <- dwtest(model)
      
      output$hasil_autokorelasi <- renderText({
        paste("Durbin-Watson Test untuk Autokorelasi\n",
              "DW =", round(dw_test$statistic, 4), "\n",
              "p-value =", format(dw_test$p.value, scientific = TRUE), "\n\n",
              "Interpretasi DW:\n",
              "DW ≈ 2: Tidak ada autokorelasi\n",
              "DW < 2: Autokorelasi positif\n",
              "DW > 2: Autokorelasi negatif\n\n",
              "Kesimpulan:",
              if(dw_test$p.value < 0.05) {
                "PELANGGARAN: Terdapat autokorelasi (p < 0.05)"
              } else {
                "TERPENUHI: Tidak ada autokorelasi signifikan (p >= 0.05)"
              })
      })
      
      values$autokorelasi <- dw_test
    }, error = function(e) {
      output$hasil_autokorelasi <- renderText("Error dalam uji Durbin-Watson")
    })
    
    # 4. Uji Non-Multikolinearitas (VIF)
    tryCatch({
      if(length(values$regresi_var_x) > 1) {
        vif_values <- vif(model)
        
        output$hasil_multikolinearitas <- renderText({
          vif_text <- paste(capture.output({
            cat("Variance Inflation Factor (VIF):\n")
            for(i in 1:length(vif_values)) {
              cat(paste0(names(vif_values)[i], ": ", round(vif_values[i], 3), "\n"))
            }
          }), collapse = "")
          
          max_vif <- max(vif_values)
          
          paste(vif_text, "\n",
                "Interpretasi VIF:\n",
                "VIF < 5: Tidak ada multikolinearitas\n",
                "5 ≤ VIF < 10: Multikolinearitas sedang\n",
                "VIF ≥ 10: Multikolinearitas tinggi\n\n",
                "VIF Maksimum:", round(max_vif, 3), "\n\n",
                "Kesimpulan:",
                if(max_vif >= 10) {
                  "PELANGGARAN: Multikolinearitas tinggi terdeteksi"
                } else if(max_vif >= 5) {
                  "PERINGATAN: Multikolinearitas sedang terdeteksi"
                } else {
                  "TERPENUHI: Tidak ada masalah multikolinearitas"
                })
        })
        
        values$multikolinearitas <- vif_values
      } else {
        output$hasil_multikolinearitas <- renderText("VIF tidak dapat dihitung untuk model dengan satu prediktor")
      }
    }, error = function(e) {
      output$hasil_multikolinearitas <- renderText("Error dalam perhitungan VIF")
    })
    
    # Interpretasi Keseluruhan Asumsi
    output$interpretasi_asumsi_regresi <- renderText({
      interpretasi <- "**RINGKASAN PEMERIKSAAN ASUMSI REGRESI**\n\n"
      
      # Normalitas
      if (!is.null(values$normalitas_residual)) {
        if (values$normalitas_residual$p.value < 0.05) {
          interpretasi <- paste0(interpretasi, 
                                 "- **Normalitas residual**: Tidak terpenuhi (p < 0.05). Error tidak menyebar normal, yang dapat memengaruhi keakuratan hasil.\n")
        } else {
          interpretasi <- paste0(interpretasi, 
                                 "- **Normalitas residual**: Terpenuhi (p ≥ 0.05). Error menyebar normal, sesuai dengan asumsi regresi.\n")
        }
      }
      
      # Homoskedastisitas
      if (!is.null(values$homoskedastisitas)) {
        if (values$homoskedastisitas$p.value < 0.05) {
          interpretasi <- paste0(interpretasi, 
                                 "- **Homoskedastisitas**: Tidak terpenuhi (p < 0.05). Terdapat ketidaksamaan varians error (heteroskedastisitas).\n")
        } else {
          interpretasi <- paste0(interpretasi, 
                                 "- **Homoskedastisitas**: Terpenuhi (p ≥ 0.05). Sebaran varians residual merata.\n")
        }
      }
      
      # Autokorelasi
      if (!is.null(values$autokorelasi)) {
        if (values$autokorelasi$p.value < 0.05) {
          interpretasi <- paste0(interpretasi, 
                                 "- **Autokorelasi**: Tidak terpenuhi (p < 0.05). Terdapat pola keterkaitan antar residual, indikasi data tidak independen.\n")
        } else {
          interpretasi <- paste0(interpretasi, 
                                 "- **Autokorelasi**: Terpenuhi (p ≥ 0.05). Tidak ditemukan pola keterkaitan antar residual.\n")
        }
      }
      
      # Multikolinearitas
      if (!is.null(values$multikolinearitas)) {
        max_vif <- max(values$multikolinearitas)
        if (max_vif >= 10) {
          interpretasi <- paste0(interpretasi, 
                                 "- **Multikolinearitas**: Tidak terpenuhi (VIF ≥ 10). Terdapat hubungan sangat kuat antar variabel prediktor.\n")
        } else if (max_vif >= 5) {
          interpretasi <- paste0(interpretasi, 
                                 "- **Multikolinearitas**: Perlu perhatian (5 ≤ VIF < 10). Ada hubungan sedang antar variabel.\n")
        } else {
          interpretasi <- paste0(interpretasi, 
                                 "- **Multikolinearitas**: Terpenuhi (VIF < 5). Tidak ada masalah berarti antar variabel prediktor.\n")
        }
      }
      
      # Saran Perbaikan
      interpretasi <- paste0(interpretasi, "\n**SARAN PERBAIKAN (jika ada asumsi yang tidak terpenuhi):**\n")
      interpretasi <- paste0(interpretasi, "- Gunakan transformasi data (misal logaritma atau akar kuadrat) untuk memperbaiki distribusi atau varians.\n")
      interpretasi <- paste0(interpretasi, "- Jika terdapat heteroskedastisitas, gunakan pendekatan regresi robust.\n")
      interpretasi <- paste0(interpretasi, "- Jika residual tidak normal, pertimbangkan metode bootstrap.\n")
      interpretasi <- paste0(interpretasi, "- Jika terjadi multikolinearitas, hilangkan atau gabungkan variabel yang saling berkorelasi.\n")
      interpretasi <- paste0(interpretasi, "- Jika ada autokorelasi, pertimbangkan penggunaan model runtun waktu atau penyesuaian model lainnya.\n")
      
      interpretasi
    })
  })
  
  # ===== DOWNLOAD HANDLERS LENGKAP =====
  # Helper function untuk membuat laporan PDF yang lebih komprehensif
  create_pdf_report <- function(title, content, interpretation = NULL, plot_func = NULL, additional_info = NULL) {
    temp_file <- tempfile(fileext = ".pdf")
    
    pdf(temp_file, width = 8.5, height = 11)
    
    # Halaman judul
    par(mar = c(1, 1, 1, 1))
    plot.new()
    
    # Header dengan logo/branding
    rect(0.05, 0.85, 0.95, 0.95, col = "#109f9e", border = NA)
    text(0.5, 0.9, "DASHBOARD KERENTANAN SOSIAL", cex = 2, font = 2, col = "white", adj = 0.5)
    
    # Judul laporan
    text(0.5, 0.75, title, cex = 2.5, font = 2, adj = 0.5, col = "#2c3e50")
    lines(c(0.15, 0.85), c(0.7, 0.7), lwd = 4, col = "#109f9e")
    
    # Informasi tanggal dan waktu
    text(0.5, 0.6, paste("Tanggal Laporan:", format(Sys.Date(), "%d %B %Y")), cex = 1.4, adj = 0.5)
    text(0.5, 0.55, paste("Waktu Pembuatan:", format(Sys.time(), "%H:%M:%S WIB")), cex = 1.2, adj = 0.5, col = "gray40")
    
    # Informasi tambahan jika ada
    if(!is.null(additional_info)) {
      text(0.5, 0.45, additional_info, cex = 1.1, adj = 0.5, col = "gray30")
    }
    
    # Footer
    text(0.5, 0.15, "Laporan Analisis Statistik", cex = 1.6, font = 3, adj = 0.5, col = "#109f9e")
    text(0.5, 0.1, "Sistem Informasi Kerentanan Sosial", cex = 1.2, adj = 0.5, col = "gray50")
    text(0.5, 0.05, "Halaman 1", cex = 0.9, adj = 0.5, col = "gray50")
    
    # Halaman konten hasil analisis
    if(!is.null(content)) {
      plot.new()
      par(mar = c(1, 1, 1, 1))
      
      # Header halaman
      rect(0.05, 0.92, 0.95, 0.98, col = "#f8f9fa", border = "#dee2e6", lwd = 2)
      text(0.07, 0.95, "HASIL ANALISIS STATISTIK", cex = 1.8, font = 2, adj = 0, col = "#2c3e50")
      
      # Konten hasil
      content_lines <- strsplit(content, "\n")[[1]]
      max_lines <- 35
      y_positions <- seq(0.88, 0.08, length.out = min(length(content_lines), max_lines))
      
      for(i in 1:min(length(content_lines), max_lines)) {
        text(0.07, y_positions[i], content_lines[i], cex = 0.9, adj = c(0, 1), family = "mono")
      }
      
      # Jika ada lebih banyak konten, buat halaman tambahan
      if(length(content_lines) > max_lines) {
        remaining_lines <- content_lines[(max_lines + 1):length(content_lines)]
        y_positions_2 <- seq(0.88, 0.08, length.out = min(length(remaining_lines), max_lines))
        
        for(i in 1:min(length(remaining_lines), max_lines)) {
          text(0.07, y_positions_2[i], remaining_lines[i], cex = 0.9, adj = c(0, 1), family = "mono")
        }
      }
      
      text(0.5, 0.02, "Halaman 2", cex = 0.9, adj = 0.5, col = "gray50")
    }
    
    # Halaman plot/visualisasi
    if(!is.null(plot_func)) {
      plot.new()
      par(mar = c(1, 1, 1, 1))
      
      # Header halaman
      rect(0.05, 0.92, 0.95, 0.98, col = "#f8f9fa", border = "#dee2e6", lwd = 2)
      text(0.07, 0.95, "VISUALISASI DATA", cex = 1.8, font = 2, adj = 0, col = "#2c3e50")
      
      # Plot area
      par(fig = c(0.1, 0.9, 0.15, 0.85), new = TRUE)
      par(mar = c(4, 4, 4, 2))
      tryCatch({
        plot_func()
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, "Error dalam membuat plot", cex = 1.5, adj = 0.5)
      })
      
      par(fig = c(0, 1, 0, 1), new = TRUE)
      par(mar = c(1, 1, 1, 1))
      text(0.5, 0.05, "Halaman 3", cex = 0.9, adj = 0.5, col = "gray50")
    }
    
    # Halaman interpretasi dan kesimpulan
    if(!is.null(interpretation)) {
      plot.new()
      par(mar = c(1, 1, 1, 1))
      
      # Header halaman
      rect(0.05, 0.92, 0.95, 0.98, col = "#e8f4f8", border = "#109f9e", lwd = 2)
      text(0.07, 0.95, "INTERPRETASI & KESIMPULAN", cex = 1.8, font = 2, adj = 0, col = "#2c3e50")
      
      # Konten interpretasi
      # Bagi interpretasi menjadi paragraf
      interp_paragraphs <- strsplit(interpretation, "\\. ")[[1]]
      current_y <- 0.85
      
      for(para in interp_paragraphs) {
        if(current_y > 0.1) {
          # Wrap text untuk paragraf panjang
          words <- strsplit(para, " ")[[1]]
          lines <- character()
          current_line <- ""
          
          for(word in words) {
            test_line <- ifelse(current_line == "", word, paste(current_line, word))
            if(nchar(test_line) <= 80) {
              current_line <- test_line
            } else {
              if(current_line != "") lines <- c(lines, current_line)
              current_line <- word
            }
          }
          if(current_line != "") lines <- c(lines, paste0(current_line, "."))
          
          for(line in lines) {
            if(current_y > 0.1) {
              text(0.07, current_y, line, cex = 1.1, adj = c(0, 1))
              current_y <- current_y - 0.04
            }
          }
          current_y <- current_y - 0.02  # Extra space between paragraphs
        }
      }
      
      # Rekomendasi
      if(current_y > 0.25) {
        rect(0.05, current_y - 0.15, 0.95, current_y - 0.02, col = "#fff3cd", border = "#ffeaa7")
        text(0.07, current_y - 0.05, "REKOMENDASI:", cex = 1.3, font = 2, adj = 0, col = "#856404")
        text(0.07, current_y - 0.09, "• Validasi hasil dengan data tambahan jika memungkinkan", cex = 1, adj = 0)
        text(0.07, current_y - 0.12, "• Pertimbangkan faktor kontekstual dalam interpretasi", cex = 1, adj = 0)
      }
      
      text(0.5, 0.02, "Halaman 4", cex = 0.9, adj = 0.5, col = "gray50")
    }
    
    dev.off()
    return(temp_file)
  }
  
  # Download handlers untuk Kategorisasi
  output$download_kategorisasi_img <- downloadHandler(
    filename = function() {
      paste("kategorisasi_", values$kategorisasi_var, "_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      jpeg(file, width = 1200, height = 800, quality = 95)
      barplot(values$kategorisasi_result,
              main = paste("Distribusi Kategori", values$kategorisasi_var),
              col = c("#2ecc71", "#f39c12", "#e74c3c"), 
              ylab = "Jumlah Daerah",
              xlab = "Kategori",
              cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)
      
      # Tambahkan nilai di atas bar
      bp <- barplot(values$kategorisasi_result, plot = FALSE)
      text(bp, values$kategorisasi_result + max(values$kategorisasi_result) * 0.02,
           values$kategorisasi_result, cex = 1.2, font = 2)
      dev.off()
    }
  )
  
  output$download_kategorisasi_report <- downloadHandler(
    filename = function() {
      paste("laporan_kategorisasi_", values$kategorisasi_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      content <- paste("HASIL KATEGORISASI VARIABEL:", values$kategorisasi_var, "\n\n",
                       "Metode Kategorisasi:", values$kategorisasi_method, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "DISTRIBUSI KATEGORI:\n",
                       "- Kategori Rendah:", values$kategorisasi_result[1], "daerah (",
                       round(values$kategorisasi_result[1]/sum(values$kategorisasi_result)*100, 1), "%)\n",
                       "- Kategori Sedang:", values$kategorisasi_result[2], "daerah (",
                       round(values$kategorisasi_result[2]/sum(values$kategorisasi_result)*100, 1), "%)\n",
                       "- Kategori Tinggi:", values$kategorisasi_result[3], "daerah (",
                       round(values$kategorisasi_result[3]/sum(values$kategorisasi_result)*100, 1), "%)\n\n",
                       "BATAS KATEGORI:\n",
                       "- Rendah: ≤", round(values$kategorisasi_breaks[2], 3), "\n",
                       "- Sedang:", round(values$kategorisasi_breaks[2], 3), "-", round(values$kategorisasi_breaks[3], 3), "\n",
                       "- Tinggi: >", round(values$kategorisasi_breaks[3], 3), "\n\n",
                       "STATISTIK DESKRIPTIF:\n",
                       "- Total Daerah:", sum(values$kategorisasi_result), "\n",
                       "- Kategori Dominan:", names(values$kategorisasi_result)[which.max(values$kategorisasi_result)])
      
      plot_func <- function() {
        barplot(values$kategorisasi_result,
                main = paste("Distribusi Kategori", values$kategorisasi_var),
                col = c("#2ecc71", "#f39c12", "#e74c3c"), 
                ylab = "Jumlah Daerah",
                xlab = "Kategori")
        
        # Tambahkan nilai di atas bar
        bp <- barplot(values$kategorisasi_result, plot = FALSE)
        text(bp, values$kategorisasi_result + max(values$kategorisasi_result) * 0.02,
             values$kategorisasi_result, cex = 1.1, font = 2)
      }
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Kategorisasi -", values$kategorisasi_var),
        content = content,
        plot_func = plot_func,
        additional_info = paste("Metode:", values$kategorisasi_method)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_kategorisasi_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_kategorisasi_", values$kategorisasi_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      total_daerah <- sum(values$kategorisasi_result)
      pct_rendah <- round(values$kategorisasi_result[1]/total_daerah * 100, 1)
      pct_sedang <- round(values$kategorisasi_result[2]/total_daerah * 100, 1)
      pct_tinggi <- round(values$kategorisasi_result[3]/total_daerah * 100, 1)
      
      interpretation <- paste("Analisis kategorisasi untuk variabel", values$kategorisasi_var,
                              "telah berhasil dilakukan menggunakan metode", values$kategorisasi_method,
                              ". Proses kategorisasi ini membagi", total_daerah, "daerah menjadi tiga tingkat kerentanan: rendah, sedang, dan tinggi.",
                              "Hasil kategorisasi menunjukkan bahwa", pct_rendah, "% daerah termasuk kategori rendah,",
                              pct_sedang, "% kategori sedang, dan", pct_tinggi, "% kategori tinggi.",
                              "Distribusi ini memberikan gambaran tentang sebaran tingkat kerentanan sosial di wilayah studi.",
                              "Daerah dengan kategori tinggi memerlukan perhatian prioritas dalam program pengurangan kerentanan sosial.",
                              "Informasi ini dapat digunakan sebagai dasar untuk perencanaan dan pengambilan kebijakan",
                              "dalam upaya mengurangi kerentanan sosial di berbagai daerah.",
                              "Metode", values$kategorisasi_method, "dipilih karena kemampuannya dalam",
                              ifelse(values$kategorisasi_method == "quantile",
                                     "membagi data berdasarkan distribusi kuantil yang memastikan jumlah observasi yang relatif seimbang di setiap kategori",
                                     "membagi data berdasarkan interval yang sama sehingga memberikan interpretasi yang lebih intuitif"))
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Kategorisasi -", values$kategorisasi_var),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Total Daerah:", total_daerah, "| Metode:", values$kategorisasi_method)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_kategorisasi_all <- downloadHandler(
    filename = function() {
      paste("kategorisasi_lengkap_", values$kategorisasi_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      total_daerah <- sum(values$kategorisasi_result)
      pct_rendah <- round(values$kategorisasi_result[1]/total_daerah * 100, 1)
      pct_sedang <- round(values$kategorisasi_result[2]/total_daerah * 100, 1)
      pct_tinggi <- round(values$kategorisasi_result[3]/total_daerah * 100, 1)
      
      content <- paste("HASIL KATEGORISASI VARIABEL:", values$kategorisasi_var, "\n\n",
                       "Metode Kategorisasi:", values$kategorisasi_method, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "DISTRIBUSI KATEGORI:\n",
                       "- Kategori Rendah:", values$kategorisasi_result[1], "daerah (", pct_rendah, "%)\n",
                       "- Kategori Sedang:", values$kategorisasi_result[2], "daerah (", pct_sedang, "%)\n",
                       "- Kategori Tinggi:", values$kategorisasi_result[3], "daerah (", pct_tinggi, "%)\n\n",
                       "BATAS KATEGORI:\n",
                       "- Rendah: ≤", round(values$kategorisasi_breaks[2], 3), "\n",
                       "- Sedang:", round(values$kategorisasi_breaks[2], 3), "-", round(values$kategorisasi_breaks[3], 3), "\n",
                       "- Tinggi: >", round(values$kategorisasi_breaks[3], 3))
      
      interpretation <- paste("Analisis kategorisasi untuk variabel", values$kategorisasi_var,
                              "telah berhasil dilakukan menggunakan metode", values$kategorisasi_method,
                              ". Dari", total_daerah, "daerah yang dianalisis, distribusi menunjukkan",
                              pct_rendah, "% daerah kategori rendah,", pct_sedang, "% kategori sedang, dan",
                              pct_tinggi, "% kategori tinggi. Kategorisasi ini dapat membantu identifikasi daerah",
                              "yang memerlukan perhatian khusus dalam program pengurangan kerentanan sosial.")
      
      plot_func <- function() {
        barplot(values$kategorisasi_result,
                main = paste("Distribusi Kategori", values$kategorisasi_var),
                col = c("#2ecc71", "#f39c12", "#e74c3c"), 
                ylab = "Jumlah Daerah",
                xlab = "Kategori")
        
        bp <- barplot(values$kategorisasi_result, plot = FALSE)
        text(bp, values$kategorisasi_result + max(values$kategorisasi_result) * 0.02,
             values$kategorisasi_result, cex = 1.1, font = 2)
      }
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap Kategorisasi -", values$kategorisasi_var),
        content = content,
        interpretation = interpretation,
        plot_func = plot_func,
        additional_info = paste("Total Daerah:", total_daerah, "| Metode:", values$kategorisasi_method)
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk Statistik Deskriptif
  output$download_deskriptif_report <- downloadHandler(
    filename = function() {
      paste("laporan_deskriptif_", values$deskriptif_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$deskriptif_result)) return()
      
      pdf(file, width = 8.5, height = 11)
      
      # Halaman judul
      par(mar = c(1, 1, 1, 1))
      plot.new()
      
      # Header
      rect(0.05, 0.85, 0.95, 0.95, col = "#109f9e", border = NA)
      text(0.5, 0.9, "DASHBOARD KERENTANAN SOSIAL", cex = 2, font = 2, col = "white", adj = 0.5)
      
      # Judul laporan
      text(0.5, 0.75, paste("Laporan Statistik Deskriptif -", values$deskriptif_var), cex = 2.5, font = 2, adj = 0.5, col = "#2c3e50")
      lines(c(0.15, 0.85), c(0.7, 0.7), lwd = 4, col = "#109f9e")
      
      # Informasi tanggal
      text(0.5, 0.6, paste("Tanggal Laporan:", format(Sys.Date(), "%d %B %Y")), cex = 1.4, adj = 0.5)
      text(0.5, 0.55, paste("Waktu Pembuatan:", format(Sys.time(), "%H:%M:%S WIB")), cex = 1.2, adj = 0.5, col = "gray40")
      
      # Konten hasil
      text(0.5, 0.45, "HASIL STATISTIK DESKRIPTIF:", cex = 1.6, font = 2, adj = 0.5)
      
      y_pos <- 0.35
      for(i in 1:nrow(values$deskriptif_result)) {
        text(0.3, y_pos, paste(values$deskriptif_result$Statistik[i], ":"), cex = 1.2, adj = 0)
        text(0.7, y_pos, round(values$deskriptif_result$Nilai[i], 4), cex = 1.2, adj = 1, font = 2)
        y_pos <- y_pos - 0.05
      }
      
      # Footer
      text(0.5, 0.05, "Halaman 1", cex = 0.9, adj = 0.5, col = "gray50")
      
      dev.off()
    }
  )
  
  output$download_deskriptif_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_deskriptif_", values$deskriptif_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$deskriptif_result)) return()
      
      var_data <- data_variabel[[values$deskriptif_var]]
      mean_val <- round(mean(var_data, na.rm = TRUE), 3)
      median_val <- round(median(var_data, na.rm = TRUE), 3)
      sd_val <- round(sd(var_data, na.rm = TRUE), 3)
      cv <- round(sd_val/mean_val * 100, 1)
      
      pdf(file, width = 8.5, height = 11)
      
      par(mar = c(1, 1, 1, 1))
      plot.new()
      
      # Header
      rect(0.05, 0.92, 0.95, 0.98, col = "#e8f4f8", border = "#109f9e", lwd = 2)
      text(0.07, 0.95, "INTERPRETASI STATISTIK DESKRIPTIF", cex = 1.8, font = 2, adj = 0, col = "#2c3e50")
      
      # Interpretasi
      interpretation <- paste("Analisis statistik deskriptif untuk variabel", values$deskriptif_var,
                              "menunjukkan karakteristik distribusi data yang penting untuk dipahami.",
                              "Nilai rata-rata sebesar", mean_val, "dengan standar deviasi", sd_val,
                              "mengindikasikan tingkat variabilitas data. Koefisien variasi sebesar", cv, "%",
                              "menunjukkan", ifelse(cv < 20, "variabilitas rendah", ifelse(cv < 50, "variabilitas sedang", "variabilitas tinggi")),
                              "dalam dataset.")
      
      # Bagi interpretasi menjadi paragraf
      interp_paragraphs <- strsplit(interpretation, "\\. ")[[1]]
      current_y <- 0.85
      
      for(para in interp_paragraphs) {
        if(current_y > 0.1) {
          # Wrap text untuk paragraf panjang
          words <- strsplit(para, " ")[[1]]
          lines <- character()
          current_line <- ""
          
          for(word in words) {
            test_line <- ifelse(current_line == "", word, paste(current_line, word))
            if(nchar(test_line) <= 80) {
              current_line <- test_line
            } else {
              if(current_line != "") lines <- c(lines, current_line)
              current_line <- word
            }
          }
          if(current_line != "") lines <- c(lines, paste0(current_line, "."))
          
          for(line in lines) {
            if(current_y > 0.1) {
              text(0.07, current_y, line, cex = 1.1, adj = c(0, 1))
              current_y <- current_y - 0.04
            }
          }
          current_y <- current_y - 0.02
        }
      }
      
      text(0.5, 0.02, "Halaman 1", cex = 0.9, adj = 0.5, col = "gray50")
      
      dev.off()
    }
  )
  
  output$download_deskriptif_all <- downloadHandler(
    filename = function() {
      paste("deskriptif_lengkap_", values$deskriptif_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$deskriptif_result)) return()
      
      var_data <- data_variabel[[values$deskriptif_var]]
      mean_val <- round(mean(var_data, na.rm = TRUE), 3)
      sd_val <- round(sd(var_data, na.rm = TRUE), 3)
      cv <- round(sd_val/mean_val * 100, 1)
      
      pdf(file, width = 8.5, height = 11)
      
      # Halaman 1: Judul dan Hasil
      par(mar = c(1, 1, 1, 1))
      plot.new()
      
      # Header
      rect(0.05, 0.85, 0.95, 0.95, col = "#109f9e", border = NA)
      text(0.5, 0.9, "DASHBOARD KERENTANAN SOSIAL", cex = 2, font = 2, col = "white", adj = 0.5)
      
      # Judul
      text(0.5, 0.75, paste("Laporan Lengkap Statistik Deskriptif\n", values$deskriptif_var), cex = 2, font = 2, adj = 0.5, col = "#2c3e50")
      
      # Hasil
      text(0.5, 0.6, "HASIL STATISTIK DESKRIPTIF:", cex = 1.6, font = 2, adj = 0.5)
      
      y_pos <- 0.5
      for(i in 1:nrow(values$deskriptif_result)) {
        text(0.3, y_pos, paste(values$deskriptif_result$Statistik[i], ":"), cex = 1.2, adj = 0)
        text(0.7, y_pos, round(values$deskriptif_result$Nilai[i], 4), cex = 1.2, adj = 1, font = 2)
        y_pos <- y_pos - 0.05
      }
      
      # Halaman 2: Interpretasi
      plot.new()
      
      rect(0.05, 0.92, 0.95, 0.98, col = "#e8f4f8", border = "#109f9e", lwd = 2)
      text(0.07, 0.95, "INTERPRETASI & KESIMPULAN", cex = 1.8, font = 2, adj = 0, col = "#2c3e50")
      
      interpretation <- paste("Analisis statistik deskriptif untuk variabel", values$deskriptif_var,
                              "menunjukkan karakteristik distribusi data. Dengan rata-rata", mean_val,
                              "dan standar deviasi", sd_val, ", data menunjukkan variabilitas",
                              ifelse(cv < 20, "rendah", ifelse(cv < 50, "sedang", "tinggi")),
                              ". Informasi ini penting untuk analisis lanjutan.")
      
      # Tampilkan interpretasi
      current_y <- 0.85
      words <- strsplit(interpretation, " ")[[1]]
      lines <- character()
      current_line <- ""
      
      for(word in words) {
        test_line <- ifelse(current_line == "", word, paste(current_line, word))
        if(nchar(test_line) <= 80) {
          current_line <- test_line
        } else {
          if(current_line != "") lines <- c(lines, current_line)
          current_line <- word
        }
      }
      if(current_line != "") lines <- c(lines, current_line)
      
      for(line in lines) {
        if(current_y > 0.1) {
          text(0.07, current_y, line, cex = 1.1, adj = c(0, 1))
          current_y <- current_y - 0.04
        }
      }
      
      dev.off()
    }
  )
  
  output$download_grafik_report <- downloadHandler(
    filename = function() {
      paste("laporan_grafik_", values$grafik_var, "_", values$grafik_type, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      var_data <- data_variabel[[values$grafik_var]]
      
      content <- paste("LAPORAN VISUALISASI DATA\n",
                       "Variabel:", values$grafik_var, "\n",
                       "Jenis Grafik:", values$grafik_type, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "STATISTIK DASAR:\n",
                       "Mean:", round(mean(var_data, na.rm = TRUE), 4), "\n",
                       "Median:", round(median(var_data, na.rm = TRUE), 4), "\n",
                       "Std Dev:", round(sd(var_data, na.rm = TRUE), 4), "\n",
                       "Min:", round(min(var_data, na.rm = TRUE), 4), "\n",
                       "Max:", round(max(var_data, na.rm = TRUE), 4), "\n",
                       "N:", length(var_data[!is.na(var_data)]))
      
      plot_func <- function() {
        if(values$grafik_type == "Histogram") {
          hist(var_data, main = paste("Histogram", values$grafik_var),
               xlab = values$grafik_var, col = "lightblue", breaks = 20,
               ylab = "Frekuensi")
          abline(v = mean(var_data, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
          legend("topright", legend = "Mean", col = "red", lty = 2, lwd = 2)
        } else if(values$grafik_type == "Box Plot") {
          boxplot(var_data, main = paste("Box Plot", values$grafik_var),
                  ylab = values$grafik_var, col = "lightgreen")
          points(1, mean(var_data, na.rm = TRUE), col = "red", pch = 19, cex = 1.5)
          legend("topright", legend = "Mean", col = "red", pch = 19)
        } else if(values$grafik_type == "Density Plot") {
          plot(density(var_data, na.rm = TRUE), main = paste("Density Plot", values$grafik_var),
               xlab = values$grafik_var, col = "red", lwd = 2)
          polygon(density(var_data, na.rm = TRUE), col = rgb(1,0,0,0.3))
          abline(v = mean(var_data, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
          abline(v = median(var_data, na.rm = TRUE), col = "green", lwd = 2, lty = 2)
          legend("topright", legend = c("Mean", "Median"), col = c("blue", "green"), lty = 2, lwd = 2)
        }
      }
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Visualisasi -", values$grafik_var),
        content = content,
        plot_func = plot_func,
        additional_info = paste("Jenis:", values$grafik_type)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_grafik_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_grafik_", values$grafik_var, "_", values$grafik_type, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      var_data <- data_variabel[[values$grafik_var]]
      
      interpretation <- ""
      if(values$grafik_type == "Histogram") {
        interpretation <- paste("Histogram untuk variabel", values$grafik_var,
                                "menunjukkan distribusi frekuensi data. Bentuk distribusi dapat mengindikasikan normalitas data dan adanya outlier.",
                                "Garis merah menunjukkan nilai rata-rata. Jika distribusi simetris, histogram akan berbentuk lonceng.",
                                "Jika condong ke kanan (right-skewed), ekor distribusi memanjang ke kanan.",
                                "Jika condong ke kiri (left-skewed), ekor distribusi memanjang ke kiri.",
                                "Informasi ini penting untuk menentukan metode analisis statistik yang tepat.")
      } else if(values$grafik_type == "Box Plot") {
        q1 <- quantile(var_data, 0.25, na.rm = TRUE)
        q3 <- quantile(var_data, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        outliers <- sum(var_data < (q1 - 1.5*iqr) | var_data > (q3 + 1.5*iqr), na.rm = TRUE)
        interpretation <- paste("Box plot untuk variabel", values$grafik_var,
                                "menunjukkan distribusi kuartil data. Terdapat", outliers, "outlier potensial dalam data.",
                                "Titik merah menunjukkan nilai rata-rata. Box plot membantu mengidentifikasi:",
                                "1) Median (garis tengah dalam box), 2) Kuartil pertama dan ketiga (batas box),",
                                "3) Nilai minimum dan maksimum (whiskers), 4) Outlier (titik di luar whiskers).",
                                "Jika median berada di tengah box, distribusi relatif simetris.")
      } else {
        interpretation <- paste("Density plot untuk variabel", values$grafik_var,
                                "menunjukkan estimasi distribusi probabilitas data. Garis biru menunjukkan mean, garis hijau menunjukkan median.",
                                "Bentuk kurva dapat mengindikasikan karakteristik distribusi data:",
                                "1) Kurva berbentuk lonceng mengindikasikan distribusi normal,",
                                "2) Kurva dengan satu puncak (unimodal) vs multiple puncak (multimodal),",
                                "3) Skewness dapat dilihat dari posisi relatif mean dan median.",
                                "Density plot berguna untuk memahami bentuk distribusi secara visual.")
      }
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Grafik -", values$grafik_var),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Jenis:", values$grafik_type, "| N:", length(var_data[!is.na(var_data)]))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_grafik_all <- downloadHandler(
    filename = function() {
      paste("grafik_lengkap_", values$grafik_var, "_", values$grafik_type, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      var_data <- data_variabel[[values$grafik_var]]
      
      content <- paste("LAPORAN VISUALISASI DATA\n",
                       "Variabel:", values$grafik_var, "\n",
                       "Jenis Grafik:", values$grafik_type, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "STATISTIK DASAR:\n",
                       "Mean:", round(mean(var_data, na.rm = TRUE), 4), "\n",
                       "Median:", round(median(var_data, na.rm = TRUE), 4), "\n",
                       "Std Dev:", round(sd(var_data, na.rm = TRUE), 4), "\n",
                       "Min:", round(min(var_data, na.rm = TRUE), 4), "\n",
                       "Max:", round(max(var_data, na.rm = TRUE), 4), "\n",
                       "N:", length(var_data[!is.na(var_data)]))
      
      interpretation <- paste("Visualisasi data untuk variabel", values$grafik_var,
                              "menggunakan", values$grafik_type, "memberikan insight tentang distribusi dan karakteristik data.",
                              "Analisis visual ini penting untuk memahami pola data sebelum melakukan analisis statistik lanjutan.")
      
      plot_func <- function() {
        if(values$grafik_type == "Histogram") {
          hist(var_data, main = paste("Histogram", values$grafik_var),
               xlab = values$grafik_var, col = "lightblue", breaks = 20,
               ylab = "Frekuensi")
          abline(v = mean(var_data, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
          legend("topright", legend = "Mean", col = "red", lty = 2, lwd = 2)
        } else if(values$grafik_type == "Box Plot") {
          boxplot(var_data, main = paste("Box Plot", values$grafik_var),
                  ylab = values$grafik_var, col = "lightgreen")
          points(1, mean(var_data, na.rm = TRUE), col = "red", pch = 19, cex = 1.5)
          legend("topright", legend = "Mean", col = "red", pch = 19)
        } else if(values$grafik_type == "Density Plot") {
          plot(density(var_data, na.rm = TRUE), main = paste("Density Plot", values$grafik_var),
               xlab = values$grafik_var, col = "red", lwd = 2)
          polygon(density(var_data, na.rm = TRUE), col = rgb(1,0,0,0.3))
          abline(v = mean(var_data, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
          abline(v = median(var_data, na.rm = TRUE), col = "green", lwd = 2, lty = 2)
          legend("topright", legend = c("Mean", "Median"), col = c("blue", "green"), lty = 2, lwd = 2)
        }
      }
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap Visualisasi -", values$grafik_var),
        content = content,
        interpretation = interpretation,
        plot_func = plot_func,
        additional_info = paste("Jenis:", values$grafik_type, "| N:", length(var_data[!is.na(var_data)]))
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk Peta
  output$download_peta_img <- downloadHandler(
    filename = function() {
      paste("peta_", values$peta_var, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Untuk peta leaflet, kita buat plot alternatif
      png(file, width = 1200, height = 800)
      
      if(!is.null(values$peta_data)) {
        plot(values$peta_data$lng, values$peta_data$lat,
             col = colorRampPalette(c("blue", "red"))(10)[cut(values$peta_data$value, 10)],
             pch = 19, cex = 2,
             main = paste("Peta Distribusi", values$peta_var),
             xlab = "Longitude", ylab = "Latitude",
             cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)
        
        # Tambahkan legend
        legend("topright",
               legend = c("Rendah", "Sedang", "Tinggi"),
               col = c("blue", "yellow", "red"),
               pch = 19, cex = 1.2)
      }
      
      dev.off()
    }
  )
  
  output$download_peta_report <- downloadHandler(
    filename = function() {
      paste("laporan_peta_", values$peta_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      var_data <- data_variabel[[values$peta_var]]
      
      content <- paste("LAPORAN VISUALISASI PETA\n",
                       "Variabel:", values$peta_var, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "STATISTIK SPASIAL:\n",
                       "Jumlah Lokasi:", nrow(data_variabel), "\n",
                       "Nilai Minimum:", round(min(var_data, na.rm = TRUE), 4), "\n",
                       "Nilai Maksimum:", round(max(var_data, na.rm = TRUE), 4), "\n",
                       "Rata-rata:", round(mean(var_data, na.rm = TRUE), 4), "\n",
                       "Standar Deviasi:", round(sd(var_data, na.rm = TRUE), 4), "\n\n",
                       "DESKRIPSI PETA:\n",
                       "Peta menampilkan distribusi spasial variabel dengan menggunakan\n",
                       "sistem warna untuk menunjukkan variasi nilai antar lokasi.\n",
                       "Warna biru menunjukkan nilai rendah, sedangkan warna merah\n",
                       "menunjukkan nilai tinggi.")
      
      plot_func <- function() {
        if(!is.null(values$peta_data)) {
          plot(values$peta_data$lng, values$peta_data$lat,
               col = colorRampPalette(c("blue", "red"))(10)[cut(values$peta_data$value, 10)],
               pch = 19, cex = 1.5,
               main = paste("Peta Distribusi", values$peta_var),
               xlab = "Longitude", ylab = "Latitude")
          
          legend("topright",
                 legend = c("Rendah", "Sedang", "Tinggi"),
                 col = c("blue", "yellow", "red"),
                 pch = 19)
        }
      }
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Peta -", values$peta_var),
        content = content,
        plot_func = plot_func,
        additional_info = paste("Lokasi:", nrow(data_variabel))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_peta_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_peta_", values$peta_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      var_data <- data_variabel[[values$peta_var]]
      min_val <- round(min(var_data, na.rm = TRUE), 3)
      max_val <- round(max(var_data, na.rm = TRUE), 3)
      
      interpretation <- paste("Peta distribusi spasial untuk variabel", values$peta_var,
                              "menampilkan variasi geografis nilai di seluruh wilayah studi.",
                              "Rentang nilai dari", min_val, "hingga", max_val, "menunjukkan heterogenitas spasial yang signifikan.",
                              "Visualisasi spasial ini penting untuk mengidentifikasi:",
                              "1) Kluster atau pengelompokan nilai tinggi/rendah (spatial clustering),",
                              "2) Pola distribusi spasial (random, clustered, atau dispersed),",
                              "3) Hotspot dan coldspot dalam distribusi variabel,",
                              "4) Hubungan spasial antar wilayah yang berdekatan.",
                              "Informasi spasial ini dapat digunakan untuk perencanaan kebijakan yang berbasis lokasi",
                              "dan mempertimbangkan karakteristik geografis dalam intervensi program.",
                              "Analisis lebih lanjut dapat meliputi autokorelasi spasial dan analisis tetangga terdekat.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Peta -", values$peta_var),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Range:", min_val, "-", max_val, "| N:", nrow(data_variabel))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_peta_all <- downloadHandler(
    filename = function() {
      paste("peta_lengkap_", values$peta_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      var_data <- data_variabel[[values$peta_var]]
      min_val <- round(min(var_data, na.rm = TRUE), 3)
      max_val <- round(max(var_data, na.rm = TRUE), 3)
      
      content <- paste("LAPORAN VISUALISASI PETA\n",
                       "Variabel:", values$peta_var, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "STATISTIK SPASIAL:\n",
                       "Jumlah Lokasi:", nrow(data_variabel), "\n",
                       "Nilai Minimum:", min_val, "\n",
                       "Nilai Maksimum:", max_val, "\n",
                       "Rata-rata:", round(mean(var_data, na.rm = TRUE), 4), "\n",
                       "Standar Deviasi:", round(sd(var_data, na.rm = TRUE), 4))
      
      interpretation <- paste("Peta distribusi spasial variabel", values$peta_var,
                              "menunjukkan variasi geografis dengan rentang nilai", min_val, "hingga", max_val,
                              ". Visualisasi ini membantu identifikasi pola spasial dan dapat digunakan untuk perencanaan berbasis lokasi.")
      
      plot_func <- function() {
        if(!is.null(values$peta_data)) {
          plot(values$peta_data$lng, values$peta_data$lat,
               col = colorRampPalette(c("blue", "red"))(10)[cut(values$peta_data$value, 10)],
               pch = 19, cex = 1.5,
               main = paste("Peta Distribusi", values$peta_var),
               xlab = "Longitude", ylab = "Latitude")
          
          legend("topright",
                 legend = c("Rendah", "Sedang", "Tinggi"),
                 col = c("blue", "yellow", "red"),
                 pch = 19)
        }
      }
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap Peta -", values$peta_var),
        content = content,
        interpretation = interpretation,
        plot_func = plot_func,
        additional_info = paste("Range:", min_val, "-", max_val, "| N:", nrow(data_variabel))
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk Tabel
  output$download_tabel_report <- downloadHandler(
    filename = function() {
      paste("laporan_tabel_", values$tabel_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$tabel_result)) return()
      
      content <- paste("LAPORAN TABEL DATA\n",
                       "Variabel:", values$tabel_var, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "DATA RANKING:\n",
                       paste(capture.output({
                         for(i in 1:min(10, nrow(values$tabel_result))) {
                           cat(paste0(i, ". ", values$tabel_result$Kode_Daerah[i], ": ",
                                      round(values$tabel_result[[4]][i], 4), "\n"))
                         }
                         if(nrow(values$tabel_result) > 10) {
                           cat("... (dan seterusnya)\n")
                         }
                       }), collapse = ""), "\n\n",
                       "STATISTIK RINGKASAN:\n",
                       "Total Daerah:", nrow(values$tabel_result), "\n",
                       "Nilai Tertinggi:", round(max(values$tabel_result[[4]], na.rm = TRUE), 4), "\n",
                       "Nilai Terendah:", round(min(values$tabel_result[[4]], na.rm = TRUE), 4), "\n",
                       "Rata-rata:", round(mean(values$tabel_result[[4]], na.rm = TRUE), 4))
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Tabel -", values$tabel_var),
        content = content,
        additional_info = paste("Total Daerah:", nrow(values$tabel_result))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_tabel_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_tabel_", values$tabel_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$tabel_result)) return()
      
      var_data <- data_variabel[[values$tabel_var]]
      n_data <- length(var_data[!is.na(var_data)])
      mean_val <- round(mean(var_data, na.rm = TRUE), 3)
      
      # Identifikasi daerah dengan nilai tertinggi dan terendah
      max_idx <- which.max(var_data)
      min_idx <- which.min(var_data)
      max_region <- data_variabel$districtcode[max_idx]
      min_region <- data_variabel$districtcode[min_idx]
      
      interpretation <- paste("Tabel ranking untuk variabel", values$tabel_var,
                              "menampilkan urutan", n_data, "daerah berdasarkan nilai dari tertinggi ke terendah.",
                              "Daerah dengan nilai tertinggi adalah", max_region,
                              "yang menunjukkan karakteristik paling menonjol untuk variabel ini.",
                              "Sebaliknya, daerah dengan nilai terendah adalah", min_region,
                              "yang mungkin memerlukan perhatian khusus atau intervensi.",
                              "Rata-rata nilai sebesar", mean_val, "dapat digunakan sebagai benchmark",
                              "untuk mengkategorikan daerah di atas atau di bawah rata-rata.",
                              "Ranking ini berguna untuk:",
                              "1) Identifikasi daerah prioritas untuk intervensi,",
                              "2) Alokasi sumber daya berdasarkan kebutuhan,",
                              "3) Monitoring dan evaluasi program,",
                              "4) Benchmarking antar daerah.",
                              "Informasi ini dapat diintegrasikan dengan data lain untuk analisis yang lebih komprehensif.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Tabel -", values$tabel_var),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Top:", max_region, "| Bottom:", min_region, "| Mean:", mean_val)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_tabel_all <- downloadHandler(
    filename = function() {
      paste("tabel_lengkap_", values$tabel_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$tabel_result)) return()
      
      var_data <- data_variabel[[values$tabel_var]]
      n_data <- length(var_data[!is.na(var_data)])
      mean_val <- round(mean(var_data, na.rm = TRUE), 3)
      max_region <- data_variabel$districtcode[which.max(var_data)]
      min_region <- data_variabel$districtcode[which.min(var_data)]
      
      content <- paste("LAPORAN TABEL DATA\n",
                       "Variabel:", values$tabel_var, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "DATA RANKING (Top 10):\n",
                       paste(capture.output({
                         for(i in 1:min(10, nrow(values$tabel_result))) {
                           cat(paste0(i, ". ", values$tabel_result$Kode_Daerah[i], ": ",
                                      round(values$tabel_result[[4]][i], 4), "\n"))
                         }
                       }), collapse = ""), "\n\n",
                       "STATISTIK RINGKASAN:\n",
                       "Total Daerah:", n_data, "\n",
                       "Daerah Tertinggi:", max_region, "\n",
                       "Daerah Terendah:", min_region, "\n",
                       "Rata-rata:", mean_val)
      
      interpretation <- paste("Tabel ranking variabel", values$tabel_var,
                              "menunjukkan", max_region, "sebagai daerah dengan nilai tertinggi dan",
                              min_region, "sebagai daerah dengan nilai terendah.",
                              "Informasi ini dapat digunakan untuk prioritas kebijakan dan alokasi sumber daya.")
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap Tabel -", values$tabel_var),
        content = content,
        interpretation = interpretation,
        additional_info = paste("N:", n_data, "| Range:", max_region, "-", min_region)
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk Uji Normalitas
  output$download_uji_normal_report <- downloadHandler(
    filename = function() {
      paste("laporan_uji_normalitas_", values$uji_normal_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$uji_normal_result)) return()
      
      content <- paste("LAPORAN UJI NORMALITAS\n",
                       "Variabel:", values$uji_normal_var, "\n",
                       "Metode:", values$uji_normal_method, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "HASIL UJI:\n",
                       if(values$uji_normal_method == "shapiro") {
                         paste("Shapiro-Wilk Test\n",
                               "W =", round(values$uji_normal_result$statistic, 4), "\n",
                               "p-value =", format(values$uji_normal_result$p.value, scientific = TRUE), "\n")
                       } else {
                         paste("Kolmogorov-Smirnov Test\n",
                               "D =", round(values$uji_normal_result$statistic, 4), "\n",
                               "p-value =", format(values$uji_normal_result$p.value, scientific = TRUE), "\n")
                       }, "\n",
                       "HIPOTESIS:\n",
                       "H0: Data berdistribusi normal\n",
                       "H1: Data tidak berdistribusi normal\n",
                       "α = 0.05\n\n",
                       "KESIMPULAN:\n",
                       if(values$uji_normal_result$p.value < 0.05) {
                         "Tolak H0: Data TIDAK berdistribusi normal (p < 0.05)"
                       } else {
                         "Terima H0: Data berdistribusi normal (p >= 0.05)"
                       })
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Uji Normalitas -", values$uji_normal_var),
        content = content,
        additional_info = paste("Metode:", values$uji_normal_method, "| p-value:", format(values$uji_normal_result$p.value, scientific = TRUE))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_uji_homo_report <- downloadHandler(
    filename = function() {
      paste("laporan_uji_homogenitas_", values$uji_homo_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$uji_homo_result)) return()
      
      content <- paste("LAPORAN UJI HOMOGENITAS\n",
                       "Variabel:", values$uji_homo_var, "\n",
                       "Metode:", values$uji_homo_method, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "HASIL UJI:\n",
                       if(values$uji_homo_method == "levene") {
                         paste("Levene Test\n",
                               "F =", round(values$uji_homo_result$`F value`[1], 4), "\n",
                               "p-value =", format(values$uji_homo_result$`Pr(>F)`[1], scientific = TRUE), "\n")
                       } else {
                         paste("Bartlett Test\n",
                               "K-squared =", round(values$uji_homo_result$statistic, 4), "\n",
                               "p-value =", format(values$uji_homo_result$p.value, scientific = TRUE), "\n")
                       }, "\n",
                       "HIPOTESIS:\n",
                       "H0: Varians kelompok sama (homogen)\n",
                       "H1: Varians kelompok tidak sama\n",
                       "α = 0.05\n\n",
                       "KESIMPULAN:\n",
                       if(values$uji_homo_method == "levene") {
                         if(values$uji_homo_result$`Pr(>F)`[1] < 0.05) {
                           "Tolak H0: Varians kedua kelompok TIDAK homogen (p < 0.05)"
                         } else {
                           "Terima H0: Varians kedua kelompok homogen (p >= 0.05)"
                         }
                       } else {
                         if(values$uji_homo_result$p.value < 0.05) {
                           "Tolak H0: Varians kedua kelompok TIDAK homogen (p < 0.05)"
                         } else {
                           "Terima H0: Varians kedua kelompok homogen (p >= 0.05)"
                         }
                       })
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Uji Homogenitas -", values$uji_homo_var),
        content = content,
        additional_info = paste("Metode:", values$uji_homo_method)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_uji_normal_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_uji_normalitas_", values$uji_normal_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$uji_normal_result)) return()
      
      interpretation <- paste("Uji normalitas untuk variabel", values$uji_normal_var,
                              "menggunakan metode", values$uji_normal_method,
                              "menunjukkan", ifelse(values$uji_normal_result$p.value < 0.05, "pelanggaran", "pemenuhan"),
                              "asumsi normalitas. Hasil ini memiliki implikasi penting untuk analisis statistik selanjutnya.",
                              "Jika data berdistribusi normal, dapat menggunakan uji parametrik seperti t-test dan ANOVA.",
                              "Jika data tidak berdistribusi normal, pertimbangkan:",
                              "1) Transformasi data (log, square root, Box-Cox),",
                              "2) Menggunakan uji non-parametrik,",
                              "3) Bootstrap atau permutation test,",
                              "4) Robust statistical methods.",
                              "Normalitas penting untuk validitas inferensi statistik dan akurasi confidence intervals.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Uji Normalitas -", values$uji_normal_var),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Metode:", values$uji_normal_method, "| Normal:", ifelse(values$uji_normal_result$p.value >= 0.05, "Ya", "Tidak"))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_uji_homo_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_uji_homogenitas_", values$uji_homo_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$uji_homo_result)) return()
      
      p_val <- if(values$uji_homo_method == "levene") values$uji_homo_result$`Pr(>F)`[1] else values$uji_homo_result$p.value
      
      interpretation <- paste("Uji homogenitas varians untuk variabel", values$uji_homo_var,
                              "menggunakan metode", values$uji_homo_method,
                              "menunjukkan", ifelse(p_val < 0.05, "pelanggaran", "pemenuhan"),
                              "asumsi homogenitas varians. Asumsi ini penting untuk:",
                              "1) Validitas uji t-test dan ANOVA,",
                              "2) Akurasi confidence intervals,",
                              "3) Power analisis statistik.",
                              "Jika asumsi homogenitas dilanggar, pertimbangkan:",
                              "1) Welch's t-test (tidak mengasumsikan varians sama),",
                              "2) Transformasi data untuk menstabilkan varians,",
                              "3) Robust ANOVA methods,",
                              "4) Non-parametric alternatives.",
                              "Homogenitas varians memastikan bahwa perbandingan antar kelompok valid dan tidak bias.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Uji Homogenitas -", values$uji_homo_var),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Metode:", values$uji_homo_method, "| Homogen:", ifelse(p_val >= 0.05, "Ya", "Tidak"))
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk Uji Beda Rata-rata
  output$download_rata_report <- downloadHandler(
    filename = function() {
      paste("laporan_uji_rata_", values$rata_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$rata_result)) return()
      
      content <- paste("LAPORAN UJI BEDA RATA-RATA\n",
                       "Variabel:", values$rata_var, "\n",
                       if(!is.null(values$rata_var2)) paste("Variabel 2:", values$rata_var2, "\n") else "",
                       "Metode:", values$rata_method, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "HASIL UJI:\n",
                       "t =", round(values$rata_result$statistic, 4), "\n",
                       "df =", values$rata_result$parameter, "\n",
                       "p-value =", format(values$rata_result$p.value, scientific = TRUE), "\n",
                       "95% CI: [", round(values$rata_result$conf.int[1], 4), ",", round(values$rata_result$conf.int[2], 4), "]\n\n",
                       "KESIMPULAN:\n",
                       if(values$rata_result$p.value < 0.05) {
                         "Tolak H0: Terdapat PERBEDAAN SIGNIFIKAN (p < 0.05)"
                       } else {
                         "Terima H0: TIDAK terdapat perbedaan signifikan (p >= 0.05)"
                       })
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Uji Beda Rata-rata -", values$rata_var),
        content = content,
        additional_info = paste("Metode:", values$rata_method, "| p-value:", format(values$rata_result$p.value, scientific = TRUE))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_rata_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_uji_rata_", values$rata_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$rata_result)) return()
      
      interpretation <- paste("Uji beda rata-rata untuk variabel", values$rata_var,
                              "menggunakan metode", values$rata_method,
                              "menunjukkan", ifelse(values$rata_result$p.value < 0.05, "perbedaan signifikan", "tidak ada perbedaan signifikan"),
                              "secara statistik. Hasil ini memiliki implikasi praktis yang penting:",
                              "1) Jika signifikan, terdapat bukti empiris adanya perbedaan yang tidak disebabkan oleh kebetulan,",
                              "2) Confidence interval memberikan rentang estimasi perbedaan yang plausible,",
                              "3) Effect size dapat dihitung untuk menilai magnitude perbedaan,",
                              "4) Hasil ini dapat digunakan untuk pengambilan keputusan berbasis bukti.",
                              "Pertimbangkan juga practical significance selain statistical significance.",
                              "Validitas hasil bergantung pada pemenuhan asumsi normalitas dan homogenitas varians.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Uji Beda Rata-rata -", values$rata_var),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Metode:", values$rata_method, "| Signifikan:", ifelse(values$rata_result$p.value < 0.05, "Ya", "Tidak"))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_rata_all <- downloadHandler(
    filename = function() {
      paste("uji_rata_lengkap_", values$rata_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$rata_result)) return()
      
      content <- paste("LAPORAN UJI BEDA RATA-RATA\n",
                       "Variabel:", values$rata_var, "\n",
                       if(!is.null(values$rata_var2)) paste("Variabel 2:", values$rata_var2, "\n") else "",
                       "Metode:", values$rata_method, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "HASIL UJI:\n",
                       "t =", round(values$rata_result$statistic, 4), "\n",
                       "df =", values$rata_result$parameter, "\n",
                       "p-value =", format(values$rata_result$p.value, scientific = TRUE), "\n",
                       "95% CI: [", round(values$rata_result$conf.int[1], 4), ",", round(values$rata_result$conf.int[2], 4), "]")
      
      interpretation <- paste("Uji beda rata-rata menunjukkan",
                              ifelse(values$rata_result$p.value < 0.05, "perbedaan signifikan", "tidak ada perbedaan signifikan"),
                              "untuk variabel", values$rata_var, ". Hasil ini dapat digunakan untuk pengambilan keputusan berbasis bukti.")
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap Uji Beda Rata-rata -", values$rata_var),
        content = content,
        interpretation = interpretation,
        additional_info = paste("Metode:", values$rata_method, "| t:", round(values$rata_result$statistic, 4))
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk Uji Proporsi
  output$download_proporsi_report <- downloadHandler(
    filename = function() {
      paste("laporan_uji_proporsi_", values$proporsi_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$proporsi_result)) return()
      
      content <- paste("LAPORAN UJI PROPORSI\n",
                       "Variabel:", values$proporsi_var, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "PARAMETER UJI:\n",
                       "Nilai Batas:", values$proporsi_batas, "\n",
                       "Jumlah Sukses:", values$proporsi_success, "\n",
                       "Total Observasi:", values$proporsi_total, "\n",
                       "Proporsi Sampel:", round(values$proporsi_success/values$proporsi_total, 4), "\n\n",
                       "HASIL UJI:\n",
                       "X-squared =", round(values$proporsi_result$statistic, 4), "\n",
                       "p-value =", format(values$proporsi_result$p.value, scientific = TRUE), "\n",
                       "95% CI: [", round(values$proporsi_result$conf.int[1], 4), ",", round(values$proporsi_result$conf.int[2], 4), "]\n\n",
                       "KESIMPULAN:\n",
                       if(values$proporsi_result$p.value < 0.05) {
                         "Tolak H0: Proporsi BERBEDA SIGNIFIKAN dari hipotesis (p < 0.05)"
                       } else {
                         "Terima H0: Proporsi TIDAK berbeda signifikan dari hipotesis (p >= 0.05)"
                       })
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Uji Proporsi -", values$proporsi_var),
        content = content,
        additional_info = paste("Proporsi:", round(values$proporsi_success/values$proporsi_total, 4), "| N:", values$proporsi_total)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_proporsi_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_uji_proporsi_", values$proporsi_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$proporsi_result)) return()
      
      observed_prop <- values$proporsi_success/values$proporsi_total
      
      interpretation <- paste("Uji proporsi untuk variabel", values$proporsi_var,
                              "dengan nilai batas", values$proporsi_batas,
                              "menghasilkan proporsi sampel sebesar", round(observed_prop, 4),
                              ". Hasil uji menunjukkan", ifelse(values$proporsi_result$p.value < 0.05, "perbedaan signifikan", "tidak ada perbedaan signifikan"),
                              "dari proporsi hipotesis. Interpretasi praktis:",
                              "1) Proporsi sampel memberikan estimasi proporsi populasi,",
                              "2) Confidence interval menunjukkan rentang nilai yang plausible untuk proporsi populasi,",
                              "3) Hasil dapat digunakan untuk evaluasi program atau kebijakan,",
                              "4) Ukuran sampel mempengaruhi presisi estimasi.",
                              "Uji proporsi berguna untuk analisis data kategorikal dan evaluasi prevalensi karakteristik tertentu dalam populasi.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Uji Proporsi -", values$proporsi_var),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Proporsi:", round(observed_prop, 4), "| Batas:", values$proporsi_batas)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_proporsi_all <- downloadHandler(
    filename = function() {
      paste("uji_proporsi_lengkap_", values$proporsi_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$proporsi_result)) return()
      
      observed_prop <- values$proporsi_success/values$proporsi_total
      
      content <- paste("LAPORAN UJI PROPORSI\n",
                       "Variabel:", values$proporsi_var, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "PARAMETER UJI:\n",
                       "Nilai Batas:", values$proporsi_batas, "\n",
                       "Jumlah Sukses:", values$proporsi_success, "\n",
                       "Total Observasi:", values$proporsi_total, "\n",
                       "Proporsi Sampel:", round(observed_prop, 4), "\n\n",
                       "HASIL UJI:\n",
                       "X-squared =", round(values$proporsi_result$statistic, 4), "\n",
                       "p-value =", format(values$proporsi_result$p.value, scientific = TRUE), "\n",
                       "95% CI: [", round(values$proporsi_result$conf.int[1], 4), ",", round(values$proporsi_result$conf.int[2], 4), "]")
      
      interpretation <- paste("Uji proporsi menunjukkan proporsi sampel", round(observed_prop, 4),
                              "dengan", ifelse(values$proporsi_result$p.value < 0.05, "perbedaan signifikan", "tidak ada perbedaan signifikan"),
                              "dari hipotesis. Hasil ini dapat digunakan untuk evaluasi dan pengambilan keputusan.")
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap Uji Proporsi -", values$proporsi_var),
        content = content,
        interpretation = interpretation,
        additional_info = paste("Proporsi:", round(observed_prop, 4), "| N:", values$proporsi_total)
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk Uji Variance
  output$download_variance_report <- downloadHandler(
    filename = function() {
      paste("laporan_uji_variance_", values$variance_var1, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$variance_result)) return()
      
      content <- paste("LAPORAN UJI VARIANCE\n",
                       "Variabel 1:", values$variance_var1, "\n",
                       if(!is.null(values$variance_var2)) paste("Variabel 2:", values$variance_var2, "\n") else "",
                       "Metode:", values$variance_method, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "HASIL UJI:\n",
                       if(values$variance_method == "chi_square") {
                         paste("Chi-squared =", round(values$variance_result$statistic, 4), "\n",
                               "df =", values$variance_result$parameter, "\n",
                               "p-value =", format(values$variance_result$p.value, scientific = TRUE), "\n",
                               "Sample variance =", round(values$variance_result$sample_var, 4), "\n",
                               "Hypothesized variance =", values$variance_result$hypothesis_var)
                       } else if(values$variance_method == "ftest") {
                         paste("F =", round(values$variance_result$statistic, 4), "\n",
                               "df1 =", values$variance_result$parameter[1], "\n",
                               "df2 =", values$variance_result$parameter[2], "\n",
                               "p-value =", format(values$variance_result$p.value, scientific = TRUE), "\n",
                               "95% CI: [", round(values$variance_result$conf.int[1], 4), ",", round(values$variance_result$conf.int[2], 4), "]")
                       } else {
                         paste("F =", round(values$variance_result$`F value`[1], 4), "\n",
                               "df1 =", values$variance_result$Df[1], "\n",
                               "df2 =", values$variance_result$Df[2], "\n",
                               "p-value =", format(values$variance_result$`Pr(>F)`[1], scientific = TRUE))
                       }, "\n\n",
                       "KESIMPULAN:\n",
                       if(values$variance_method == "chi_square") {
                         if(values$variance_result$p.value < 0.05) {
                           "Tolak H0: Variance BERBEDA SIGNIFIKAN dari hipotesis (p < 0.05)"
                         } else {
                           "Terima H0: Variance TIDAK berbeda signifikan dari hipotesis (p >= 0.05)"
                         }
                       } else {
                         p_val <- if(values$variance_method == "ftest") values$variance_result$p.value else values$variance_result$`Pr(>F)`[1]
                         if(p_val < 0.05) {
                           "Tolak H0: Varians kedua kelompok BERBEDA SIGNIFIKAN (p < 0.05)"
                         } else {
                           "Terima H0: Varians kedua kelompok TIDAK berbeda signifikan (p >= 0.05)"
                         }
                       })
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Uji Variance -", values$variance_var1),
        content = content,
        additional_info = paste("Metode:", values$variance_method)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_variance_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_uji_variance_", values$variance_var1, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$variance_result)) return()
      
      interpretation <- paste("Uji variance untuk variabel", values$variance_var1,
                              "menggunakan metode", values$variance_method,
                              "bertujuan untuk mengevaluasi asumsi homogenitas varians yang penting dalam analisis statistik.",
                              "Variance mengukur dispersi atau sebaran data dari nilai rata-rata.",
                              "Hasil uji ini memiliki implikasi untuk:",
                              "1) Validitas uji parametrik yang mengasumsikan varians sama,",
                              "2) Pemilihan metode analisis yang tepat,",
                              "3) Interpretasi confidence intervals,",
                              "4) Power analysis dalam desain penelitian.",
                              "Jika asumsi homogenitas varians dilanggar, pertimbangkan:",
                              "- Transformasi data untuk menstabilkan varians,",
                              "- Menggunakan robust methods,",
                              "- Welch's correction untuk unequal variances,",
                              "- Non-parametric alternatives.",
                              "Homogenitas varians penting untuk validitas inferensi statistik.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Uji Variance -", values$variance_var1),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Metode:", values$variance_method)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_variance_all <- downloadHandler(
    filename = function() {
      paste("uji_variance_lengkap_", values$variance_var1, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$variance_result)) return()
      
      content <- paste("LAPORAN UJI VARIANCE\n",
                       "Variabel 1:", values$variance_var1, "\n",
                       if(!is.null(values$variance_var2)) paste("Variabel 2:", values$variance_var2, "\n") else "",
                       "Metode:", values$variance_method, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "HASIL UJI:\n",
                       if(values$variance_method == "chi_square") {
                         paste("Chi-squared =", round(values$variance_result$statistic, 4), "\n",
                               "p-value =", format(values$variance_result$p.value, scientific = TRUE))
                       } else if(values$variance_method == "ftest") {
                         paste("F =", round(values$variance_result$statistic, 4), "\n",
                               "p-value =", format(values$variance_result$p.value, scientific = TRUE))
                       } else {
                         paste("F =", round(values$variance_result$`F value`[1], 4), "\n",
                               "p-value =", format(values$variance_result$`Pr(>F)`[1], scientific = TRUE))
                       })
      
      interpretation <- paste("Uji variance menggunakan metode", values$variance_method,
                              "untuk mengevaluasi homogenitas varians. Hasil ini penting untuk validitas analisis statistik selanjutnya.")
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap Uji Variance -", values$variance_var1),
        content = content,
        interpretation = interpretation,
        additional_info = paste("Metode:", values$variance_method)
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk ANOVA
  output$download_anova_report <- downloadHandler(
    filename = function() {
      paste("laporan_anova_", values$anova_var_y, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$anova_result)) return()
      
      content <- paste("LAPORAN ANALISIS ANOVA\n",
                       "Variabel Dependen:", values$anova_var_y, "\n",
                       if(values$anova_type == "one_way") {
                         paste("Variabel Pengelompokan:", values$anova_var_group, "\n",
                               "Jumlah Kelompok:", values$anova_n_groups)
                       } else {
                         paste("Faktor 1:", values$anova_var_group1, "\n",
                               "Faktor 2:", values$anova_var_group2, "\n",
                               "Level Faktor 1:", values$anova_n_groups1, "\n",
                               "Level Faktor 2:", values$anova_n_groups2)
                       }, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "HASIL ANOVA:\n",
                       paste(capture.output(print(values$anova_result)), collapse = "\n"), "\n\n",
                       "KESIMPULAN:\n",
                       if(values$anova_type == "one_way") {
                         if(values$anova_result[[1]]$`Pr(>F)`[1] < 0.05) {
                           "Tolak H0: Terdapat PERBEDAAN SIGNIFIKAN antar kelompok (p < 0.05)"
                         } else {
                           "Terima H0: TIDAK terdapat perbedaan signifikan antar kelompok (p >= 0.05)"
                         }
                       } else {
                         paste("Faktor 1:", if(values$anova_result[[1]]$`Pr(>F)`[1] < 0.05) "Signifikan" else "Tidak signifikan", "\n",
                               "Faktor 2:", if(values$anova_result[[1]]$`Pr(>F)`[2] < 0.05) "Signifikan" else "Tidak signifikan", "\n",
                               "Interaksi:", if(values$anova_result[[1]]$`Pr(>F)`[3] < 0.05) "Signifikan" else "Tidak signifikan")
                       })
      
      temp_file <- create_pdf_report(
        title = paste("Laporan ANOVA -", values$anova_var_y),
        content = content,
        additional_info = paste("Tipe:", values$anova_type)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_anova_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_anova_", values$anova_var_y, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$anova_result)) return()
      
      interpretation <- paste("Analisis ANOVA untuk variabel", values$anova_var_y,
                              "bertujuan untuk menguji perbedaan rata-rata antar kelompok.",
                              "ANOVA (Analysis of Variance) adalah teknik statistik yang membandingkan varians antar kelompok dengan varians dalam kelompok.",
                              "Hasil analisis ini memiliki implikasi penting:",
                              "1) Jika signifikan, terdapat bukti perbedaan rata-rata antar kelompok,",
                              "2) Effect size (eta-squared) menunjukkan magnitude perbedaan,",
                              "3) Post-hoc tests diperlukan untuk mengetahui kelompok mana yang berbeda,",
                              "4) Asumsi normalitas dan homogenitas varians harus dipenuhi.",
                              "ANOVA berguna untuk:",
                              "- Evaluasi efektivitas program atau intervensi,",
                              "- Perbandingan performa antar grup,",
                              "- Identifikasi faktor yang berpengaruh signifikan,",
                              "- Dasar untuk analisis lanjutan seperti regresi.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi ANOVA -", values$anova_var_y),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Tipe:", values$anova_type, "| Variabel:", values$anova_var_y)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_anova_all <- downloadHandler(
    filename = function() {
      paste("anova_lengkap_", values$anova_var_y, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$anova_result)) return()
      
      content <- paste("LAPORAN ANALISIS ANOVA\n",
                       "Variabel Dependen:", values$anova_var_y, "\n",
                       if(values$anova_type == "one_way") {
                         paste("Variabel Pengelompokan:", values$anova_var_group, "\n",
                               "Jumlah Kelompok:", values$anova_n_groups)
                       } else {
                         paste("Faktor 1:", values$anova_var_group1, "\n",
                               "Faktor 2:", values$anova_var_group2)
                       }, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "HASIL ANOVA:\n",
                       paste(capture.output(print(values$anova_result)), collapse = "\n"))
      
      interpretation <- paste("Analisis ANOVA menunjukkan",
                              if(values$anova_type == "one_way") {
                                if(values$anova_result[[1]]$`Pr(>F)`[1] < 0.05) "perbedaan signifikan" else "tidak ada perbedaan signifikan"
                              } else {
                                "hasil untuk multiple factors"
                              },
                              "antar kelompok untuk variabel", values$anova_var_y,
                              ". Hasil ini dapat digunakan untuk pengambilan keputusan dan analisis lanjutan.")
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap ANOVA -", values$anova_var_y),
        content = content,
        interpretation = interpretation,
        additional_info = paste("Tipe:", values$anova_type)
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk Regresi - DIPERBAIKI dengan pemisahan
  output$download_regresi_img <- downloadHandler(
    filename = function() {
      paste("plot_regresi_", values$regresi_var_y, "_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      if(is.null(values$regresi_model)) return()
      
      jpeg(file, width = 1600, height = 1200, quality = 95)
      par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
      plot(values$regresi_model, which = 1:4, cex.main = 1.2, cex.lab = 1.1, cex.axis = 1.0)
      dev.off()
    }
  )
  
  output$download_regresi_report <- downloadHandler(
    filename = function() {
      paste("laporan_regresi_", values$regresi_var_y, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$regresi_model)) return()
      
      model_summary <- summary(values$regresi_model)
      
      content <- paste("LAPORAN REGRESI LINEAR BERGANDA\n",
                       "Variabel Dependen:", values$regresi_var_y, "\n",
                       "Variabel Independen:", paste(values$regresi_var_x, collapse = ", "), "\n",
                       "Formula:", values$regresi_formula, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "HASIL REGRESI:\n",
                       paste(capture.output(print(model_summary)), collapse = "\n"), "\n\n",
                       "STATISTIK MODEL:\n",
                       "R-squared:", round(model_summary$r.squared, 4), "\n",
                       "Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n",
                       "F-statistic:", round(model_summary$fstatistic[1], 4), "\n",
                       "p-value:", format(pf(model_summary$fstatistic[1],
                                             model_summary$fstatistic[2],
                                             model_summary$fstatistic[3],
                                             lower.tail = FALSE), scientific = TRUE), "\n",
                       "Residual standard error:", round(model_summary$sigma, 4))
      
      plot_func <- function() {
        par(mfrow = c(2, 2))
        plot(values$regresi_model, which = 1:4)
      }
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Model Regresi -", values$regresi_var_y),
        content = content,
        plot_func = plot_func,
        additional_info = paste("R²:", round(model_summary$r.squared, 4), "| Prediktor:", length(values$regresi_var_x))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_regresi_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_regresi_", values$regresi_var_y, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$regresi_model)) return()
      
      model_summary <- summary(values$regresi_model)
      r_squared <- model_summary$r.squared
      adj_r_squared <- model_summary$adj.r.squared
      f_stat <- model_summary$fstatistic[1]
      p_value <- pf(f_stat, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
      
      # Identifikasi variabel signifikan
      coef_p_values <- model_summary$coefficients[, 4]
      if(values$regresi_include_intercept) {
        significant_vars <- names(coef_p_values)[-1][coef_p_values[-1] < 0.05]
      } else {
        significant_vars <- names(coef_p_values)[coef_p_values < 0.05]
      }
      
      interpretation <- paste("Model regresi linear berganda untuk variabel", values$regresi_var_y,
                              "menjelaskan", round(r_squared * 100, 2), "% variasi dalam data (R² =", round(r_squared, 4), ").",
                              "Adjusted R² sebesar", round(adj_r_squared, 4), "memberikan estimasi yang lebih konservatif",
                              "dengan mempertimbangkan jumlah prediktor dalam model.",
                              "Model secara keseluruhan", ifelse(p_value < 0.05, "signifikan", "tidak signifikan"),
                              "secara statistik (p =", format(p_value, scientific = TRUE), ").",
                              "Variabel yang berpengaruh signifikan:", ifelse(length(significant_vars) > 0, paste(significant_vars, collapse = ", "), "Tidak ada"),
                              ". Interpretasi koefisien regresi:",
                              "- Setiap unit peningkatan variabel independen akan mengubah variabel dependen sebesar nilai koefisien,",
                              "- Tanda koefisien menunjukkan arah hubungan (positif/negatif),",
                              "- Signifikansi koefisien menunjukkan apakah pengaruh tersebut statistik signifikan.",
                              "Plot diagnostik harus diperiksa untuk validasi asumsi model regresi.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Model Regresi -", values$regresi_var_y),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("R²:", round(r_squared, 4), "| Signifikan:", ifelse(p_value < 0.05, "Ya", "Tidak"))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_regresi_all <- downloadHandler(
    filename = function() {
      paste("regresi_lengkap_", values$regresi_var_y, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$regresi_model)) return()
      
      model_summary <- summary(values$regresi_model)
      r_squared <- model_summary$r.squared
      p_value <- pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
      
      content <- paste("LAPORAN REGRESI LINEAR BERGANDA\n",
                       "Variabel Dependen:", values$regresi_var_y, "\n",
                       "Variabel Independen:", paste(values$regresi_var_x, collapse = ", "), "\n",
                       "Formula:", values$regresi_formula, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "STATISTIK MODEL:\n",
                       "R-squared:", round(r_squared, 4), "\n",
                       "Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n",
                       "F-statistic:", round(model_summary$fstatistic[1], 4), "\n",
                       "p-value:", format(p_value, scientific = TRUE))
      
      interpretation <- paste("Model regresi menjelaskan", round(r_squared * 100, 2), "% variasi dalam", values$regresi_var_y,
                              "dan", ifelse(p_value < 0.05, "signifikan", "tidak signifikan"), "secara statistik.",
                              "Hasil ini dapat digunakan untuk prediksi dan pemahaman hubungan antar variabel.")
      
      plot_func <- function() {
        par(mfrow = c(2, 2))
        plot(values$regresi_model, which = 1:4)
      }
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap Regresi -", values$regresi_var_y),
        content = content,
        interpretation = interpretation,
        plot_func = plot_func,
        additional_info = paste("R²:", round(r_squared, 4), "| Prediktor:", length(values$regresi_var_x))
      )
      file.copy(temp_file, file)
    }
  )
  
  # Download handlers untuk Uji Asumsi Regresi - DIPERBAIKI dengan pemisahan
  output$download_asumsi_regresi_report <- downloadHandler(
    filename = function() {
      paste("laporan_asumsi_regresi_", values$regresi_var_y, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$regresi_model)) return()
      
      content <- paste("LAPORAN UJI ASUMSI REGRESI\n",
                       "Model Regresi:", values$regresi_formula, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "1. UJI NORMALITAS RESIDUAL:\n",
                       if(!is.null(values$normalitas_residual)) {
                         paste("Shapiro-Wilk Test\n",
                               "W =", round(values$normalitas_residual$statistic, 4), "\n",
                               "p-value =", format(values$normalitas_residual$p.value, scientific = TRUE), "\n",
                               "Kesimpulan:", ifelse(values$normalitas_residual$p.value < 0.05, "PELANGGARAN", "TERPENUHI"))
                       } else {"Tidak tersedia"}, "\n\n",
                       "2. UJI HOMOSKEDASTISITAS:\n",
                       if(!is.null(values$homoskedastisitas)) {
                         paste("Breusch-Pagan Test\n",
                               "BP =", round(values$homoskedastisitas$statistic, 4), "\n",
                               "p-value =", format(values$homoskedastisitas$p.value, scientific = TRUE), "\n",
                               "Kesimpulan:", ifelse(values$homoskedastisitas$p.value < 0.05, "PELANGGARAN", "TERPENUHI"))
                       } else {"Tidak tersedia"}, "\n\n",
                       "3. UJI NON-AUTOKORELASI:\n",
                       if(!is.null(values$autokorelasi)) {
                         paste("Durbin-Watson Test\n",
                               "DW =", round(values$autokorelasi$statistic, 4), "\n",
                               "p-value =", format(values$autokorelasi$p.value, scientific = TRUE), "\n",
                               "Kesimpulan:", ifelse(values$autokorelasi$p.value < 0.05, "PELANGGARAN", "TERPENUHI"))
                       } else {"Tidak tersedia"}, "\n\n",
                       "4. UJI NON-MULTIKOLINEARITAS:\n",
                       if(!is.null(values$multikolinearitas)) {
                         paste("VIF Maksimum:", round(max(values$multikolinearitas), 3), "\n",
                               "Kesimpulan:", ifelse(max(values$multikolinearitas) >= 10, "PELANGGARAN", 
                                                     ifelse(max(values$multikolinearitas) >= 5, "PERINGATAN", "TERPENUHI")))
                       } else {"Tidak tersedia"})
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Uji Asumsi Regresi -", values$regresi_var_y),
        content = content,
        additional_info = paste("Model:", values$regresi_formula)
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_asumsi_regresi_interp <- downloadHandler(
    filename = function() {
      paste("interpretasi_asumsi_regresi_", values$regresi_var_y, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$regresi_model)) return()
      
      interpretation <- paste("Uji asumsi regresi linear berganda untuk model", values$regresi_formula,
                              "merupakan langkah kritis untuk memvalidasi keandalan hasil analisis.",
                              "Empat asumsi utama yang diuji adalah:",
                              "1. NORMALITAS RESIDUAL: Residual harus berdistribusi normal untuk validitas uji signifikansi dan confidence intervals.",
                              "2. HOMOSKEDASTISITAS: Varians residual harus konstan untuk semua nilai prediksi.",
                              "3. NON-AUTOKORELASI: Residual harus independen satu sama lain.",
                              "4. NON-MULTIKOLINEARITAS: Variabel independen tidak boleh berkorelasi tinggi.",
                              "Pelanggaran asumsi dapat menyebabkan:",
                              "- Estimasi koefisien yang bias atau tidak efisien,",
                              "- Standard error yang tidak akurat,",
                              "- Uji signifikansi yang tidak valid,",
                              "- Confidence intervals yang tidak tepat.",
                              "Jika ada pelanggaran asumsi, pertimbangkan:",
                              "- Transformasi variabel,",
                              "- Robust regression methods,",
                              "- Generalized linear models,",
                              "- Bootstrap methods untuk inference.",
                              "Validasi asumsi memastikan interpretasi hasil regresi yang akurat dan dapat diandalkan.")
      
      temp_file <- create_pdf_report(
        title = paste("Interpretasi Uji Asumsi Regresi -", values$regresi_var_y),
        content = NULL,
        interpretation = interpretation,
        additional_info = paste("Model:", values$regresi_var_y, "~", paste(values$regresi_var_x, collapse = "+"))
      )
      file.copy(temp_file, file)
    }
  )
  
  output$download_asumsi_regresi_all <- downloadHandler(
    filename = function() {
      paste("asumsi_regresi_lengkap_", values$regresi_var_y, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if(is.null(values$regresi_model)) return()
      
      content <- paste("LAPORAN LENGKAP UJI ASUMSI REGRESI\n",
                       "Model Regresi:", values$regresi_formula, "\n",
                       "Tanggal Analisis:", format(Sys.Date(), "%d %B %Y"), "\n\n",
                       "RINGKASAN HASIL UJI ASUMSI:\n",
                       "1. Normalitas Residual:", ifelse(!is.null(values$normalitas_residual) && values$normalitas_residual$p.value >= 0.05, "✅ TERPENUHI", "❌ PELANGGARAN"), "\n",
                       "2. Homoskedastisitas:", ifelse(!is.null(values$homoskedastisitas) && values$homoskedastisitas$p.value >= 0.05, "✅ TERPENUHI", "❌ PELANGGARAN"), "\n",
                       "3. Non-Autokorelasi:", ifelse(!is.null(values$autokorelasi) && values$autokorelasi$p.value >= 0.05, "✅ TERPENUHI", "❌ PELANGGARAN"), "\n",
                       "4. Non-Multikolinearitas:", ifelse(!is.null(values$multikolinearitas) && max(values$multikolinearitas) < 5, "✅ TERPENUHI", "❌ PELANGGARAN"))
      
      interpretation <- paste("Validasi asumsi regresi menunjukkan kondisi model untuk analisis", values$regresi_var_y,
                              ". Pemenuhan asumsi memastikan validitas inferensi statistik dan akurasi prediksi model.")
      
      temp_file <- create_pdf_report(
        title = paste("Laporan Lengkap Uji Asumsi Regresi -", values$regresi_var_y),
        content = content,
        interpretation = interpretation,
        additional_info = paste("Asumsi:", ifelse(
          !is.null(values$normalitas_residual) && values$normalitas_residual$p.value >= 0.05 &&
            !is.null(values$homoskedastisitas) && values$homoskedastisitas$p.value >= 0.05 &&
            !is.null(values$autokorelasi) && values$autokorelasi$p.value >= 0.05 &&
            !is.null(values$multikolinearitas) && max(values$multikolinearitas) < 5,"Semua Terpenuhi", "Ada Pelanggaran"))
      )
      file.copy(temp_file, file)
    }
  )
}

shinyApp(ui = ui, server = server)