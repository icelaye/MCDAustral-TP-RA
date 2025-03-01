import pandas as pd
import requests
from bs4 import BeautifulSoup
import time

# Función para extraer los datos de rendimiento total de un jugador
def scrape_player_performance(player_url, player_name):
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'
    }
    
    print(f"Scrapeando datos de {player_name}...")
    
    try:
        response = requests.get(player_url, headers=headers, timeout=10)
        response.raise_for_status()
        soup = BeautifulSoup(response.content, 'html.parser')
        
        # Buscar la tabla de rendimiento
        table = soup.find('table', {'class': 'items'})
        if not table:
            print(f"No se encontró la tabla de rendimiento para {player_name} ({player_url})")
            return None
        
        rows = table.find_all('tr')
        total_row = None
        
        for row in rows:
            cols = row.find_all('td')
            if len(cols) > 0 and 'Total' in row.text:
                total_row = cols
                break
        
        if not total_row:
            print(f"No se encontró la fila 'Total' en {player_name} ({player_url})")
            return None
        
        # Extraer datos de forma segura
        data = {
            'Jugador': player_name,
            'URL Rendimiento': player_url,
            'Partidos': total_row[3].text.strip() if len(total_row) > 3 else '',
            'Goles': total_row[4].text.strip() if len(total_row) > 4 else '',
            'Asistencias': total_row[5].text.strip() if len(total_row) > 5 else '',
            'Tarjetas': total_row[6].text.strip() if len(total_row) > 6 else '',
            'Minutos Jugados': total_row[-1].text.strip() if len(total_row) > 0 else ''
        }
        
        return data
    
    except requests.RequestException as e:
        print(f"Error al procesar {player_name} ({player_url}): {e}")
        return None

# Cargar el archivo CSV
file_path = "C:/Users/ivan_/transfermarkt_players_5leagues_2023.csv"
df = pd.read_csv(file_path)

# Seleccionar solo las primeras 5 filas
#df_subset = df.head(15)

# Extraer datos de rendimiento para las primeras 5 filas
performance_data = []

for _, row in df.iterrows():
    player_name = row["Jugador"]
    player_url = row["URL Rendimiento"]
    if pd.notna(player_url):
        data = scrape_player_performance(player_url, player_name)
        if data:
            performance_data.append(data)
        time.sleep(5)  # Pausa para evitar bloqueos por exceso de peticiones

# Crear DataFrame y guardar en un nuevo CSV
performance_df = pd.DataFrame(performance_data)
output_path = "C:/Users/ivan_/player_performance_data_players.csv"
performance_df.to_csv(output_path, index=False)

print(f"Datos guardados en {output_path}")