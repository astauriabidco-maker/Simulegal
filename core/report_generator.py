from fpdf import FPDF
from datetime import datetime
from typing import Dict, List, Any

class ReportGenerator:
    def __init__(self, user_name: str):
        self.user_name = user_name
        self.pdf = FPDF()
        self.pdf.set_auto_page_break(auto=True, margin=15)

    def generate(self, results: List[Dict[str, Any]], output_path: str):
        self.pdf.add_page()
        
        # Titre
        self.pdf.set_font("Arial", 'B', 16)
        self.pdf.cell(200, 10, txt="Rapport d'éligibilité Simulegal", ln=True, align='C')
        
        # Infos utilisateur
        self.pdf.set_font("Arial", size=12)
        self.pdf.ln(10)
        self.pdf.cell(200, 10, txt=f"Utilisateur : {self.user_name}", ln=True)
        self.pdf.cell(200, 10, txt=f"Date : {datetime.now().strftime('%d/%m/%Y %H:%M')}", ln=True)
        self.pdf.ln(10)

        # Résultats
        for res in results:
            if res['score'] > 0:
                self.pdf.set_font("Arial", 'B', 14)
                color = (0, 128, 0) if res['score'] == 100 else (255, 165, 0)
                self.pdf.set_text_color(*color)
                self.pdf.cell(200, 10, txt=f"{res['name']} - Score: {res['score']}%", ln=True)
                self.pdf.set_text_color(0, 0, 0)
                
                self.pdf.set_font("Arial", size=11)
                if res['applied_exception']:
                    self.pdf.set_font("Arial", 'I', 11)
                    self.pdf.cell(200, 10, txt=f"Exception appliquée : {res['applied_exception']}", ln=True)
                    self.pdf.set_font("Arial", size=11)

                if res['missing_criteria']:
                    self.pdf.cell(200, 10, txt="Critères manquants :", ln=True)
                    for crit in res['missing_criteria']:
                        self.pdf.cell(200, 10, txt=f"  - {crit}", ln=True)
                
                self.pdf.ln(5)
                self.pdf.set_font("Arial", 'B', 11)
                self.pdf.cell(200, 10, txt="Documents requis :", ln=True)
                self.pdf.set_font("Arial", size=10)
                for doc in res['documents']:
                    self.pdf.cell(200, 8, txt=f"  [ ] {doc}", ln=True)
                
                self.pdf.ln(10)

        self.pdf.output(output_path)
        return output_path
