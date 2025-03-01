import requests
from bs4 import BeautifulSoup
import pandas as pd
import time

# Definir las ligas y sus URLs para ambas temporadas
base_urls = {
    "Premier League": "https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1/plus/?saison_id=",
    "La Liga (España)": "https://www.transfermarkt.com/la-liga/startseite/wettbewerb/ES1/plus/?saison_id=",
    "Ligue 1 Uber Eats (Francia)": "https://www.transfermarkt.com/ligue-1/startseite/wettbewerb/FR1/plus/?saison_id=",
    "Serie A Italia": "https://www.transfermarkt.com/serie-a/startseite/wettbewerb/IT1/plus/?saison_id=",
    "Bundesliga": "https://www.transfermarkt.com/bundesliga/startseite/wettbewerb/L1/plus/?saison_id=",
    "Primeira Liga (Portugal)": "https://www.transfermarkt.com/liga-portugal/startseite/wettbewerb/PO1/plus/?saison_id=",
    "Russian Premier League": "https://www.transfermarkt.com/premier-liga/startseite/wettbewerb/RU1/plus/?saison_id=",
    "cinch Premiership (Escocia)": "https://www.transfermarkt.com/premiership/startseite/wettbewerb/SC1/plus/?saison_id=",
    "Eredivisie (Holanda)": "https://www.transfermarkt.com/eredivisie/startseite/wettbewerb/NL1/plus/?saison_id=",
    "Jupiler Pro League (Bélgica)": "https://www.transfermarkt.com/jupiler-pro-league/startseite/wettbewerb/BE1/plus/?saison_id="
}

# Temporadas que queremos scrapear
seasons = {
    "2022-2023": "2022",
    "2023-2024": "2023"
}

headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'}

# Función para scrapear los datos de una temporada específica
def scrape_season(season_year, file_name):
    all_data = []

    for league, base_url in base_urls.items():
        url = base_url + season_year
        print(f"Scrapeando datos de {league} para la temporada {season_year}...")
        
        try:
            response = requests.get(url, headers=headers)
            response.raise_for_status()
            soup = BeautifulSoup(response.content, 'html.parser')
            
            # Buscar la tabla de equipos
            table = soup.find('table', {'class': 'items'})
            
            if not table:
                print(f"No se encontró la tabla de equipos en {league}")
                continue
            
            # Iterar sobre los equipos
            for row in table.find_all('tr')[1:]:
                cols = row.find_all('td')
                if len(cols) > 1:
                    try:
                        team_name = cols[1].text.strip()
                        team_url = 'https://www.transfermarkt.com' + cols[1].find('a')['href']
                        print(f"Obteniendo jugadores de {team_name}...")
                        
                        # Acceder a la página del equipo para obtener jugadores
                        team_response = requests.get(team_url, headers=headers)
                        team_response.raise_for_status()
                        team_soup = BeautifulSoup(team_response.content, 'html.parser')
                        player_table = team_soup.find('table', {'class': 'items'})
                        
                        if not player_table:
                            print(f"No se encontró la tabla de jugadores para {team_name}")
                            continue
                        
                        for player_row in player_table.find_all('tr')[1:]:
                            player_cols = player_row.find_all('td')
                            if len(player_cols) > 1:
                                try:
                                    player_info = player_cols[1].text.strip().split('\n')
                                    player_name = player_info[0].strip()
                                    player_position = player_cols[4].text.strip() if len(player_cols) > 4 else ""
                                    market_value = player_cols[-1].text.strip()
                                    
                                    # Evitar duplicados verificando si el jugador ya está en los datos
                                    if not any(d["Jugador"] == player_name and d["Equipo"] == team_name for d in all_data):
                                        all_data.append({
                                            "Temporada": season_year,
                                            "Liga": league,
                                            "Equipo": team_name,
                                            "Jugador": player_name,
                                            "Posición": player_position,
                                            "Valor de Mercado": market_value
                                        })
                                except Exception as e:
                                    print(f"Error obteniendo datos de un jugador en {team_name}: {e}")
                        
                        # Pausa para evitar bloqueos
                        time.sleep(2)
                        
                    except Exception as e:
                        print(f"Error obteniendo datos de {team_name}: {e}")
        except requests.exceptions.RequestException as e:
            print(f"Error al acceder a la URL de {league}: {e}")
            continue
    
    # Crear DataFrame y guardar en CSV
    df = pd.DataFrame(all_data)
    df.to_csv(file_name, index=False, encoding='utf-8-sig')
    print(f"Datos guardados en {file_name}")

# Ejecutar el scraping para ambas temporadas
scrape_season(seasons["2022-2023"], "transfermarkt_players_5leagues_2023.csv")
scrape_season(seasons["2023-2024"], "transfermarkt_players_5leagues_2024.csv")
