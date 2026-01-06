from fpdf import FPDF
from datetime import datetime
import os
from core.i18n import BACKEND_TRANSLATIONS

try:
    import arabic_reshaper
    from bidi.algorithm import get_display
except ImportError:
    arabic_reshaper = None
    get_display = None

class SimulegalReport(FPDF):
    def __init__(self, user_data: dict, top_match: dict, all_results: list, lang: str = "fr"):
        super().__init__()
        self.user_data = user_data
        self.top_match = top_match
        self.all_results = all_results
        self.lang = lang if lang in BACKEND_TRANSLATIONS else "fr"
        self.t = BACKEND_TRANSLATIONS[self.lang]
        
        # Registration des polices Unicode
        font_path = os.path.join(os.path.dirname(__file__), "fonts")
        self.add_font("NotoSans", "", os.path.join(font_path, "NotoSans-Regular.ttf"))
        self.add_font("NotoArabic", "", os.path.join(font_path, "NotoSansArabic-Regular.ttf"))
        
        # Sélection de la police par défaut selon la langue
        self.default_font = "NotoArabic" if self.lang == "ar" else "NotoSans"
        self.align = "R" if self.lang == "ar" else "L"
        self.set_margins(15, 15, 15)

    def _prepare_text(self, text: str) -> str:
        """Prépare le texte pour l'affichage (notamment l'Arabe)."""
        if not text: return ""
        if self.lang == "ar" and arabic_reshaper and get_display:
            reshaped_text = arabic_reshaper.reshape(text)
            bidi_text = get_display(reshaped_text)
            return bidi_text
        return text

    def header(self):
        # Logo text
        self.set_font(self.default_font, '', 20)
        self.set_text_color(99, 102, 241) # Primary color
        self.cell(0, 10, 'Simulegal', align='C', new_x="LMARGIN", new_y="NEXT")
        
        # Subtitle
        self.set_font(self.default_font, '', 10)
        self.set_text_color(100, 100, 100)
        self.cell(0, 10, self._prepare_text(self.t["report_title"]), align='C', new_x="LMARGIN", new_y="NEXT")
        
        # Line break
        self.ln(5)
        self.set_draw_color(200, 200, 200)
        self.line(15, 35, 195, 35)
        self.ln(10)

    def footer(self):
        self.set_y(-15)
        self.set_font(self.default_font, '', 8)
        self.set_text_color(128, 128, 128)
        # Footer text contains page number, let's keep it simple
        footer_text = f'{self.t["footer"]} - Page {self.page_no()}'
        self.cell(0, 10, self._prepare_text(footer_text), align='C')

    def chapter_title(self, label):
        self.set_font(self.default_font, '', 14)
        self.set_fill_color(245, 247, 255) # Light blue bg
        self.set_text_color(30, 41, 59) # Dark text
        # Use a small width for padding simulation
        self.cell(self.epw, 10, f"  {self._prepare_text(label)}", new_x="LMARGIN", new_y="NEXT", fill=True, align=self.align)
        self.ln(4)

    def body(self):
        self.add_page()
        
        # 1. Situation
        self.chapter_title(f"1. {self.t['user_situation']}")
        self.set_font(self.default_font, '', 11)
        self.set_text_color(50, 50, 50)
        
        # Format user data
        nationality = self.user_data.get("nationality", "Inconnue")
        residence = f"{self.user_data.get('residence_duration_months', 0)} mois"
        
        situation_text = f"{self.t['category']} : {nationality}\n{residence}"
        self.multi_cell(self.epw, 7, self._prepare_text(situation_text), align=self.align)
        self.ln(5)

        # 2. Résultat Principal
        self.chapter_title(f"2. {self.t['eligibility_results']}")
        
        if self.top_match and self.top_match.get('score', 0) > 0:
            score = self.top_match['score']
            self.set_font(self.default_font, '', 16)
            
            if score == 100: self.set_text_color(16, 185, 129)
            elif score >= 50: self.set_text_color(245, 158, 11)
            else: self.set_text_color(239, 68, 68)
                
            score_text = f"{self.t['score']} : {score}%"
            self.cell(self.epw, 10, self._prepare_text(score_text), new_x="LMARGIN", new_y="NEXT", align=self.align)
            
            # Nom procédure
            self.set_font(self.default_font, '', 12)
            self.set_text_color(0, 0, 0)
            self.multi_cell(self.epw, 8, self._prepare_text(self.top_match['name']), align=self.align)
            self.ln(2)
            
            # 3. Documents
            self.chapter_title(f"3. {self.t['documents']}")
            self.set_font(self.default_font, '', 11)
            self.set_text_color(0, 0, 0)
            
            documents = self.top_match.get('documents', [])
            for doc in documents:
                # Use simple bullet - for Arabic
                bullet = "-" if self.lang == "ar" else "•"
                self.multi_cell(self.epw, 7, self._prepare_text(f"{bullet} {doc}"), align=self.align)
        
        else:
            self.set_font(self.default_font, '', 12)
            self.cell(self.epw, 10, "...", new_x="LMARGIN", new_y="NEXT", align=self.align)

    def get_pdf_bytes(self):
        self.body()
        return bytes(self.output())
