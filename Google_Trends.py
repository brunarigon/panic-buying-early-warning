import time
import random
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pytrends.request import TrendReq

# Defining User-Agents for connection rotation (Session Amnesia)
user_agents = [
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36',
    'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36'
]

# Function to initialize connection and avoid IP blocks
def create_connection():
    ua = random.choice(user_agents)
    return TrendReq(hl='pt-BR', tz=180, timeout=(10,60), retries=5, backoff_factor=10, requests_args={'headers': {'User-Agent': ua}})
### hl = idioma; tz = fuso horário; 
### timeout = quanto tempo deve-se esperar antes de dar erro; 
### retries = quantas tentativas falhadas serão rodadas antes de desistir; 
### backoff = a cada tentativa falha, o código irá esperar um tempo exponencialmente maior para tentar novamente

print("Everything is ready. Let's start!")

# --------------------------------- RESEARCH CONFIGURATION - CONTROL PANEL ---------------------------------

# Search parameters
search_term = 'Chuva' ### termo para nomear o arquivo
kw_list = ['/m/06mb1'] ### termo de busca no Google
geo_loc = 'BR-SC'

# Timeframe parameters
start_date_str = '2018-01-01'
end_date_str = '2025-12-31'

# --------------------------------- FOLDER CONFIGURATION FOR GENERATED FILES ---------------------------------

# Configure dynamic folder for each topic
folder_name = f"{search_term.replace(' ', '')}_{start_date_str}_to_{end_date_str}"
script_directory = os.path.dirname(os.path.abspath(__file__)) ### definição do caminho absoluto para que a pasta seja criada no mesmo local do script
validation_path = os.path.join(script_directory, 'results', folder_name)

# Define backup path
backup_filename = f'backup_{search_term.replace(" ", "")}_{start_date_str}_{end_date_str}.pkl'
backup_path = os.path.join(validation_path, backup_filename)

# State control variables
raw_df_list = []
processed_windows = 0

# SCENARIO 1: Folder does not exist.
if not os.path.exists(validation_path):
    os.makedirs(validation_path)
    print(f"New collection started. Folder created: {validation_path}.")

# SCENARIO 2: Folder exists. Check for backup.
else:
    print(f"Accessing existing folder: {validation_path}.")
    print(f"Verifying existing files for {search_term} from {start_date_str} to {end_date_str}...")
    if os.path.exists(backup_path):
        # SCENARIO 2.A: Backup exists.
        print(f"Matching backup found. Loading data from: {backup_filename}")
        try:
            raw_df_list = pd.read_pickle(backup_path)
            processed_windows = len(raw_df_list)
            print(f"Resuming from window {processed_windows + 1}.")
        except Exception as e:
            print(f"Critical error reading backup ({e}). File might be corrupted.")
            print("Recommendation: Delete the folder manually and restart the process.")
            exit()
    else:
        # SCENARIO 2.B: Folder exists, but no backup found.
        print(f"Folder exists, but no backup found. Starting clean collection...")
        for file in os.listdir(validation_path):
            if file.endswith(".csv"):
                try:
                    os.remove(os.path.join(validation_path, file))
                except:
                    pass
    
# Opening connection to Google
pytrends = create_connection()
print(f"Starting data collection for term {search_term} on Google Trends for {geo_loc} within the period {start_date_str} to {end_date_str}.")

# --------------------------------- STAGE 1: MASTER MONTHLY SERIES ---------------------------------
print("INITIALIZING STAGE 1: Collecting master monthly series...")

try:
    pytrends.build_payload(kw_list, cat=0, timeframe=f'{start_date_str} {end_date_str}', geo=geo_loc) ### cat=0: Define a categoria da busca: 0 indica "Todas as categorias"
    master_series = pytrends.interest_over_time() ### linha que faz o download da info: conecta-se aos servidores do Google, envia o formulário preenchido acima e recebe o dataframe
        
    if 'isPartial' in master_series.columns: ### O Google costuma adicionar essa coluna para avisar quando o último dado é incompleto
        del master_series['isPartial']
    
    master_series = master_series.resample('MS').mean()

    # Saving master monthly series
    master_series_filename = '1_master_monthly_series.csv'
    master_series_path = os.path.join(validation_path, master_series_filename)
    master_series.to_csv(master_series_path, sep=';', decimal=',', float_format='%.2f')
    print("Master monthly series saved successfully. Showing the first 5 rows:")
    print(master_series.head())

except Exception as e:
    print(f"Fatal error in master monthly series. Stop and rotate IP. Details: {e}")
    exit() 
### Encerra o script se falhar aqui

print("Stage 1 (master monthly series) completed successfully.")

# --------------------------------- STAGE 2: DAILY SERIES ---------------------------------
print("INITIALIZING STAGE 2: Collecting daily series...")

# Window generator (90 days with 15 days of overlap)
current_start_date = datetime.strptime(start_date_str, "%Y-%m-%d")
final_end_date = datetime.strptime(end_date_str, "%Y-%m-%d")
window_size_days = 90
overlap_days = 15
time_windows = []

while current_start_date < final_end_date:
    current_end_date = current_start_date + timedelta(days=window_size_days) ### A data final deste pedaço será a data de início mais 90 dias
    if current_end_date > final_end_date:
        current_end_date = final_end_date ### Se ao somar 90 dias nós ultrapassarmos a data limite, corte exatamente na data limite
        
    t_str = f"{current_start_date.strftime('%Y-%m-%d')} {current_end_date.strftime('%Y-%m-%d')}"
    time_windows.append((current_start_date, current_end_date, t_str))
    current_start_date = current_end_date - timedelta(days=overlap_days) ### Para começar o próximo corte, pegue a data onde este corte terminou e volte 15 dias

    if current_end_date == final_end_date:
        break

# Verify existing backups
if os.path.exists(backup_path):
    print(f"Backup found with {len(raw_df_list)} windows processed.")
    remaining_windows = time_windows[len(raw_df_list):]
else:
    remaining_windows = time_windows

# Collection loop
print("Initializing collection loop...")
for i, (c_start, c_end, timeframe_str) in enumerate(remaining_windows):
    print(f"Collecting Window {len(raw_df_list)+1}/{len(time_windows)}: {timeframe_str}")
    success = False
    attempts = 0

    while not success and attempts < 20: ### Enquanto não uma amostra válida na cesta E não tiver tentado mais de 20 vezes no total, continue tentando
        try:
            pytrends = create_connection()
            pytrends.build_payload(kw_list, cat=0, timeframe=timeframe_str, geo=geo_loc) ### Preenchimento do formulário do Google Trends
            df_temp = pytrends.interest_over_time()

            ### Para solucionar problema de meses sem resultados
            if df_temp.empty:
                print(f"Window {timeframe_str} returned empty. Filling with zeros.")
                dates = pd.date_range(start=c_start, end=c_end, freq='D')
                window_df = pd.DataFrame({'value': 0.0}, index=dates)
                window_df.index.name = 'date'
            else:
                series_value = df_temp[kw_list].astype(float)
                window_df = pd.DataFrame(series_value)
                window_df.columns = ['value']
                window_df.index.name = 'date'

            raw_df_list.append(window_df)
            pd.to_pickle(raw_df_list, backup_path)
            success = True
           
            # Checkpoint file:
            checkpoint2_filename = f"2_daily_series_{i}_{timeframe_str.replace(' ', '_')}.csv"
            checkpoint2_path = os.path.join(validation_path, checkpoint2_filename)
            window_df.to_csv(checkpoint2_path, sep=';', decimal=',')
            print(f"Checkpoint file for the period {timeframe_str} saved successfully.")          

            # Security pause
            time.sleep(np.random.randint(40, 80))

        except Exception as e:
            print(f"Connection error (attempt {attempts}): {e}. Next attempt in 2 minutes...")
            time.sleep(120)
            attempts += 1
            
    ### Se sair do 'while' e ainda for False, significa que falhou as 20 vezes
    if not success:
        print(f"CRITICAL FAILURE collecting window {timeframe_str} after 20 attempts.")
    
    else:
        # BATCH PAUSE: Cooldown strategy to avoid accumulated rate limits (Error 429)
        if (i + 1) % 5 == 0:
            long_pause = np.random.randint(180, 240)
            print(f"Batch of 5 windows completed. Initiating a long cooldown of {long_pause} seconds to reset Google's limits...")
            time.sleep(long_pause)
                       
print("Stage 2 (daily series) completed successfully.")

# --------------------------------- STAGE 3: DATA STITCHING AND OVERLAP REMOVAL ---------------------------------
print("INITIALIZING STAGE 3: Stitching data and removing overlap...")

### Trava de segurança caso a etapa 2 tenha falhado
if not raw_df_list:
    raise ValueError("No data collected.")

### Preparando a lista para o Relatório Geral de Fatores
adjustment_factors_report = []
stitched_df = raw_df_list[0][['value']].copy()
stitched_df['value'] = stitched_df['value'].astype(float)

# Stitching loop
for i in range(1, len(raw_df_list)):
    current_df = raw_df_list[i][['value']].copy()
    current_df['value'] = current_df['value'].astype(float)
    
    # Identifying the overlap period
    overlap_start = current_df.index.min() ### The day the NEW window starts
    overlap_end = stitched_df.index.max() ### The day the OLD window ends

    accumulated_overlap_data = stitched_df.loc[overlap_start:overlap_end].copy()
    new_window_overlap_data = current_df.loc[overlap_start:overlap_end].copy()
    
    ### Caso identifique uma falta de sobreposição (onde ela deveria ocorrer)
    if accumulated_overlap_data.empty or new_window_overlap_data.empty:
        stitched_df = pd.concat([stitched_df, current_df])
        continue

    # Adjustment factor calculation
    previous_mean = accumulated_overlap_data['value'].mean()
    current_mean = new_window_overlap_data['value'].mean()
    
    if current_mean == 0 or np.isnan(current_mean):
        adjustment_factor = 1.0 ### Evita divisão por zero
    else:
        adjustment_factor = previous_mean / current_mean

    # Adjustment factors report
    adjustment_factors_report.append({
        'Window_Index': i,
        'Overlap_Period': f"{overlap_start.strftime('%Y-%m-%d')} to {overlap_end.strftime('%Y-%m-%d')}",
        'Accumulated_Series_Mean': previous_mean,
        'New_Window_Mean': current_mean,
        'Applied_Factor': adjustment_factor
    })

    # Checkpoint file:
    checkpoint3_df = pd.DataFrame()
    checkpoint3_df['Accumulated_Overlap (A)'] = accumulated_overlap_data['value'] ### vem com toda a série histórica
    checkpoint3_df['New_Overlap (B)'] = new_window_overlap_data['value'] ### é do mês novo que vai ser 'costurado' a série histórica, ele que deve ser ajustado para se adequar ao histórico
    checkpoint3_df['Mean_A'] = previous_mean
    checkpoint3_df['Mean_B'] = current_mean
    checkpoint3_df['Calculated_Factor (A/B)'] = adjustment_factor

    checkpoint3_filename = f"3_overlap_window_{i}_factor_{adjustment_factor:.2f}.csv"
    checkpoint3_path = os.path.join(validation_path, checkpoint3_filename)
    checkpoint3_df.to_csv(checkpoint3_path, sep=';', decimal=',', float_format='%.4f')
    print(f"Adjustment factor checkpoint file for Window {i} saved successfully.")
    
    # Adjust scale of the current window
    ### Multiplica toda a janela nova (não só o overlap, mas os 90 dias dela) pelo fator. Agora a janela nova está na mesma escala da antiga.
    current_df['value'] = current_df['value'] * adjustment_factor
    ### Concatena e resolve duplicatas pela média
    ### O groupby index mean resolve o problema das datas duplicadas no overlap
    combined_df = pd.concat([stitched_df, current_df]) ### Cola a janela nova (já ajustada) no final da tabela mestre
    stitched_df = combined_df.groupby(level=0)['value'].mean().to_frame() ### Para resolver as duplicatas, agrupamento pela data. Se houver duas linhas para o dia, tirar a média delas.

    ### Isso suaviza a "emenda" entre as janelas, deixando o gráfico contínuo e sem degraus artificiais.

# Adjustment factors file
adjustment_factors_df = pd.DataFrame(adjustment_factors_report)
adjustment_factors_filename = '3_adjustment_factors.csv'
adjustment_factors_path = os.path.join(validation_path, adjustment_factors_filename)
adjustment_factors_df.to_csv(adjustment_factors_path, sep=';', decimal=',', float_format='%.4f')
print("Adjustment factors report saved successfully.")

# Stitched series file (before final normalization)
stitched_series_filename = '3_stitched_series_pre_normalization.csv'
stitched_series_path = os.path.join(validation_path, stitched_series_filename)
stitched_df.to_csv(stitched_series_path, sep=';', decimal=',', float_format='%.2f')
print("Stitched series file saved successfully.")

print("Stage 3 (data stitching and overlap removal) completed successfully.")

# --------------------------------- STAGE 4: NORMALIZING DAILY DATA VIA MASTER SERIES ---------------------------------
print("INITIALIZING STAGE 4: Adjusting long-term trend...")

# Reloading the master series
master_series_path = os.path.join(validation_path, '1_master_monthly_series.csv')
master_series_cleaned = pd.read_csv(master_series_path, index_col=0, parse_dates=True, sep=';', decimal=',')

master_aligned = master_series_cleaned.iloc[:, 0]
daily_resampled_monthly = stitched_df.resample('MS')['value'].mean() ### MS é month start

# Aligning indices
common_index = master_aligned.index.intersection(daily_resampled_monthly.index) ### Verifica quais meses existem nos dois arquivos
master_aligned = master_aligned.loc[common_index]
daily_aligned = daily_resampled_monthly.loc[common_index] ### Extrai as colunas de dados apenas para esses meses coincidentes

# Calculating the monthly correction factor
correction_factor = master_aligned.squeeze() / daily_aligned.squeeze() ### Divide o valor da Série Mestra Mensal pelo valor da média diária -> o fator será o fator multiplicador para deixar todos na mesma escala
correction_factor = correction_factor.fillna(1).replace([np.inf, -np.inf], 1) ### Limpeza de erros matemáticos para valores zerados

# Checkpoint file:
df_monthly_check = pd.DataFrame({
    'Google_Master_Series (A)': master_aligned.squeeze(),
    'My_Monthly_Series (B)': daily_aligned.squeeze(),
    'Calculated_Weight (A/B)': correction_factor
})

checkpoint4_filename = '4_monthly_factor_check.csv'
checkpoint4_path = os.path.join(validation_path, checkpoint4_filename)
df_monthly_check.to_csv(checkpoint4_path, sep=';', decimal=',', float_format='%.4f')

# Interpolating the monthly factor back to daily
### Transforma o fator mensal em um fator diário que muda aos pouquinhos dia a dia, garantindo que a correção da tendência não crie distorções artificiais na curva diária.
correction_daily = correction_factor.reindex(stitched_df.index).interpolate(method='time').fillna(method='bfill').fillna(method='ffill')

# Absolute series
absolute_df = stitched_df['value'] * correction_daily
absolute_df = absolute_df[~absolute_df.index.duplicated(keep='first')] ### Remove qualquer duplicata que tenha restado
absolute_df.name = 'Absolute_Interest'

absolute_filename = f'5_absolute_result_{search_term.replace(" ", "")}.csv'
absolute_path = os.path.join(validation_path, absolute_filename)
absolute_df.to_csv(absolute_path, sep=';', decimal=',', float_format='%.2f', header=True)
print("Absolute final series file generated successfully.")

# Min-Max rescaling
### Re-normaliza toda a série final para o teto de 100 (Padrão visual Google Trends)
rescaled_df = (absolute_df / absolute_df.max()) * 100
rescaled_df.name = 'Interest_0_to_100'

rescaled_filename = f"5_rescaled_trends_result_{search_term.replace(' ', '')}.csv"
rescaled_path = os.path.join(validation_path, rescaled_filename)
rescaled_df.to_csv(rescaled_path, sep=';', decimal=',', float_format='%.2f', header=True) 
print("Rescaled final series file generated successfully.")

print("Stage 4 (normalizing daily data via master series) completed successfully.")

print("Process completed.")