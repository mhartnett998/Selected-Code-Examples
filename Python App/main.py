import tkinter as tk
import customtkinter as CTK
from tkinter import filedialog
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt
import pandas as pd
import matplotlib.font_manager as fm

font_path = "fonts/CrimsonText-Regular.ttf"
fm.fontManager.addfont(font_path)

plt.rcParams['font.family'] = 'Crimson Text'
plt.rcParams['font.size'] = 12


COMPANY_COLORS = {
    'Blue 1': "#007680",
    'Teal 1': "#6BC6AD",
    'Green 1': "#99ca44",
    'Grey 1': "#777369",
    'Grey 2': "#a19c9b"
}


class CheckboxFrame(CTK.CTkFrame):
    def __init__(self, master, labels):
        super().__init__(master)

        self.checkboxes = []  # Store references if you need to read their values later

        for i, label in enumerate(labels):
            cb = CTK.CTkCheckBox(self, text=label)
            cb.grid(row=i, column=0, padx=10, pady=5, sticky="w")
            self.checkboxes.append(cb)

    def get(self):
        return [cb.cget("text") for cb in self.checkboxes if cb.get() == 1]

class ButtonFrame(CTK.CTkFrame):
    def __init__(self, master, text, callback=None):
        super().__init__(master)
        self.callback = callback

        self.button = CTK.CTkButton(
            self,
            text=text,
            command=self.button_clicked
        )
        self.button.grid(column=0, row=0, padx=10, pady=10)

    def button_clicked(self):
        if self.callback:
            self.callback()


class LoadButton(CTK.CTkFrame):
    def __init__(self, master, load_callback):
        super().__init__(master)

        self.load_button = CTK.CTkButton(
            self,
            text='Load CSV',
            command=load_callback
        )
        self.load_button.grid(column=0, row=0, padx=10, pady=10)

class BaseChartFrame(CTK.CTkFrame):
    def __init__(self, master):
        super().__init__(master)
        self.df = None
        self.canvas = None

    def load_csv(self):
        raise NotImplementedError("load_csv() must be implemented in subclasses")

    def update_chart(self):
        raise NotImplementedError("update_chart() must be implemented in subclasses")

    def get_figure(self):
        if self.canvas:
            return self.canvas.figure
        return None

class GroupedBarChartFrame(BaseChartFrame):
    def __init__(self, master, company_colors):
        super().__init__(master)
        self.company_colors = company_colors
        self.df = None
        self.canvas = None

        # Controls
        control_frame = CTK.CTkFrame(self)
        control_frame.pack(side="left", fill="y", padx=10, pady=10)

        CTK.CTkButton(control_frame, text="Update Chart", command=self.update_chart).pack(pady=10)

        CTK.CTkLabel(control_frame, text="X Column").pack()
        self.x_var = tk.StringVar()
        self.x_dropdown = CTK.CTkComboBox(control_frame, variable=self.x_var, values=[])
        self.x_dropdown.pack(pady=5)

        CTK.CTkLabel(control_frame, text="Y Columns (multi-select)").pack()
        self.y_listbox = tk.Listbox(control_frame, selectmode=tk.MULTIPLE, height=6)
        self.y_listbox.pack(pady=5)

        CTK.CTkLabel(control_frame, text="Chart Title").pack()
        self.title_label_entry = CTK.CTkEntry(control_frame)
        self.title_label_entry.pack()

        CTK.CTkLabel(control_frame, text="X Axis Label").pack()
        self.x_label_entry = CTK.CTkEntry(control_frame)
        self.x_label_entry.pack()

        CTK.CTkLabel(control_frame, text="Y Axis Label").pack()
        self.y_label_entry = CTK.CTkEntry(control_frame)
        self.y_label_entry.pack()

        self.show_labels_var = tk.BooleanVar(value=False)
        self.show_labels_checkbox = CTK.CTkCheckBox(
            control_frame,
            text="Show Data Labels",
            variable=self.show_labels_var
        )
        self.show_labels_checkbox.pack(pady=5)

        # Plot area
        self.plot_frame = CTK.CTkFrame(self)
        self.plot_frame.pack(side="right", fill="both", expand=True)

        CTK.CTkLabel(control_frame, text="Colors").pack(pady=(10, 0))
        self.color_frame = CTK.CTkFrame(control_frame)
        self.color_frame.pack(pady=5)
        self.color_dropdowns = []

    def load_dataframe(self, df):
        self.df = df
        cols = list(df.columns)

        self.x_dropdown.configure(values=cols)
        if cols:
            self.x_var.set(cols[0])

        self.y_listbox.delete(0, tk.END)
        for col in cols:
            self.y_listbox.insert(tk.END, col)

        self.update_chart()

    def update_chart(self):
        if self.df is None:
            return

        # Determine selected Y columns first
        x_col = self.x_var.get()
        y_cols = [self.y_listbox.get(i) for i in self.y_listbox.curselection()]
        if not x_col or not y_cols:
            return

        # Save previous color choices (if any)
        previous_colors = []
        for dd in self.color_dropdowns:
            previous_colors.append(dd.get())

        # Rebuild color selectors
        for widget in self.color_frame.winfo_children():
            widget.destroy()

        self.color_dropdowns = []
        for i in range(len(y_cols)):
            # Row frame to hold dropdown + preview square
            row = CTK.CTkFrame(self.color_frame)
            row.pack(fill="x", pady=2)

            dd = CTK.CTkComboBox(row, values=list(COMPANY_COLORS.keys()))

            # Restore previous color if available
            if i < len(previous_colors):
                dd.set(previous_colors[i])
            else:
                dd.set(list(COMPANY_COLORS.keys())[i % len(COMPANY_COLORS)])

            dd.pack(side="left", padx=5)

            # Preview square
            preview = CTK.CTkLabel(row, text="", width=20, height=20, fg_color=COMPANY_COLORS[dd.get()]
)
            preview.pack(side="left", padx=5)

            # Update preview when dropdown changes
            def update_preview(choice, preview_label=preview):
                preview_label.configure(fg_color=COMPANY_COLORS[choice])

            dd.configure(command=update_preview)

            self.color_dropdowns.append(dd)

        # Now you can safely read chosen colors
        chosen_colors = [dd.get() for dd in self.color_dropdowns]

        fig, ax = plt.subplots(figsize=(6, 4))

        x_values = self.df[x_col]
        x_positions = range(len(x_values))
        bar_width = 0.8 / len(y_cols)

        for i, col in enumerate(y_cols):
            offset = (i - len(y_cols)/2) * bar_width + bar_width/2
            ax.bar(
                [x + offset for x in x_positions],
                self.df[col],
                width=bar_width,
                label=col,
                color = COMPANY_COLORS[self.color_dropdowns[i].get()]
            )

        if self.show_labels_var.get():
            for bar in ax.containers:
                for rect in bar:
                    height = rect.get_height()
                    ax.text(
                        rect.get_x() + rect.get_width() / 2,
                        height,
                        f"{height:.2f}",
                        ha="center",
                        va="bottom" if height >= 0 else "top"
                    )

        ax.axhline(0, color="black", linewidth=1, linestyle="--")
        ax.set_xticks(list(x_positions))
        ax.set_xticklabels(x_values, rotation=45, ha="right")

        ax.set_xlabel(self.x_label_entry.get() or x_col)
        ax.set_ylabel(self.y_label_entry.get() or "Value")
        ax.set_title(self.title_label_entry.get() or "")
        ax.legend()

        if self.canvas:
            self.canvas.get_tk_widget().destroy()

        self.canvas = FigureCanvasTkAgg(fig, master=self.plot_frame)
        self.canvas.draw()
        self.canvas.get_tk_widget().pack(fill="both", expand=True)

class ScatterChartFrame(BaseChartFrame):
    def __init__(self, master, company_colors):
        super().__init__(master)
        self.company_colors = company_colors
        self.df = None
        self.canvas = None

        # Controls
        control_frame = CTK.CTkFrame(self)
        control_frame.pack(side="left", fill="y", padx=10, pady=10)

        CTK.CTkButton(control_frame, text="Update Chart", command=self.update_chart).pack(pady=10)

        CTK.CTkLabel(control_frame, text="X Column").pack()
        self.x_var = tk.StringVar()
        self.x_dropdown = CTK.CTkComboBox(control_frame, variable=self.x_var, values=[])
        self.x_dropdown.pack(pady=5)

        CTK.CTkLabel(control_frame, text="Y Column").pack()
        self.y_var = tk.StringVar()
        self.y_dropdown = CTK.CTkComboBox(control_frame, variable=self.y_var, values=[])
        self.y_dropdown.pack(pady=5)

        CTK.CTkLabel(control_frame, text="Chart Title").pack()
        self.title_label_entry = CTK.CTkEntry(control_frame)
        self.title_label_entry.pack()

        CTK.CTkLabel(control_frame, text="X Axis Label").pack()
        self.x_label_entry = CTK.CTkEntry(control_frame)
        self.x_label_entry.pack()

        CTK.CTkLabel(control_frame, text="Y Axis Label").pack()
        self.y_label_entry = CTK.CTkEntry(control_frame)
        self.y_label_entry.pack()

        self.show_labels_var = tk.BooleanVar(value=False)
        self.show_labels_checkbox = CTK.CTkCheckBox(
            control_frame,
            text="Show Data Labels",
            variable=self.show_labels_var
        )
        self.show_labels_checkbox.pack(pady=5)

        # Plot area
        self.plot_frame = CTK.CTkFrame(self)
        self.plot_frame.pack(side="right", fill="both", expand=True)

        CTK.CTkLabel(control_frame, text="Colors").pack(pady=(10, 0))
        self.color_frame = CTK.CTkFrame(control_frame)
        self.color_frame.pack(pady=5)
        self.color_dropdowns = []

    def load_dataframe(self, df):
        self.df = df
        cols = list(df.columns)

        self.x_dropdown.configure(values=cols)
        if cols:
            self.x_var.set(cols[0])

        self.y_dropdown.configure(values=cols)
        if cols:
            self.y_var.set(cols[0])

        self.update_chart()

    def update_chart(self):
        if self.df is None:
            return

        x_col = self.x_var.get()
        y_col = self.y_var.get()
        if not x_col or not y_col:
            return

        # --- Rebuild color selector (only one needed) ---
        previous_color = (
            self.color_dropdowns[0].get()
            if self.color_dropdowns
            else list(COMPANY_COLORS.keys())[0]
        )

        for widget in self.color_frame.winfo_children():
            widget.destroy()

        self.color_dropdowns = []

        row = CTK.CTkFrame(self.color_frame)
        row.pack(fill="x", pady=2)

        dd = CTK.CTkComboBox(row, values=list(COMPANY_COLORS.keys()))
        dd.set(previous_color)
        dd.pack(side="left", padx=5)

        preview = CTK.CTkLabel(row, text="", width=20, height=20, fg_color=COMPANY_COLORS[dd.get()]
)
        preview.pack(side="left", padx=5)

        def update_preview(choice, preview_label=preview):
            preview_label.configure(fg_color=COMPANY_COLORS[choice])

        dd.configure(command=update_preview)
        self.color_dropdowns.append(dd)

        # --- Create scatter plot ---
        fig, ax = plt.subplots(figsize=(6, 4))

        ax.scatter(
            self.df[x_col],
            self.df[y_col],
            color=COMPANY_COLORS[dd.get()],
            s=60  # marker size
        )

        ax.set_xlabel(self.x_label_entry.get() or x_col)
        ax.set_ylabel(self.y_label_entry.get() or y_col)
        ax.set_title(self.title_label_entry.get() or "")

        # Optional gridlines if you want to support them later
        # ax.grid(True, linestyle="--", alpha=0.5)

        # --- Render ---
        if self.canvas:
            self.canvas.get_tk_widget().destroy()

        self.canvas = FigureCanvasTkAgg(fig, master=self.plot_frame)
        self.canvas.draw()
        self.canvas.get_tk_widget().pack(fill="both", expand=True)

class LineChartFrame(BaseChartFrame):
    def __init__(self, master, company_colors):
        super().__init__(master)
        self.company_colors = company_colors
        self.df = None
        self.canvas = None

        # Controls
        control_frame = CTK.CTkFrame(self)
        control_frame.pack(side="left", fill="y", padx=10, pady=10)

        CTK.CTkButton(control_frame, text="Update Chart", command=self.update_chart).pack(pady=10)

        CTK.CTkLabel(control_frame, text="X Column").pack()
        self.x_var = tk.StringVar()
        self.x_dropdown = CTK.CTkComboBox(control_frame, variable=self.x_var, values=[])
        self.x_dropdown.pack(pady=5)

        CTK.CTkLabel(control_frame, text="Y Series (multi-select)").pack()
        self.y_listbox = tk.Listbox(control_frame, selectmode=tk.MULTIPLE, height=6)
        self.y_listbox.pack(pady=5)

        CTK.CTkLabel(control_frame, text="Chart Title").pack()
        self.title_label_entry = CTK.CTkEntry(control_frame)
        self.title_label_entry.pack()

        CTK.CTkLabel(control_frame, text="X Axis Label").pack()
        self.x_label_entry = CTK.CTkEntry(control_frame)
        self.x_label_entry.pack()

        CTK.CTkLabel(control_frame, text="Y Axis Label").pack()
        self.y_label_entry = CTK.CTkEntry(control_frame)
        self.y_label_entry.pack()

        self.show_labels_var = tk.BooleanVar(value=False)
        self.show_labels_checkbox = CTK.CTkCheckBox(
            control_frame,
            text="Show Data Labels",
            variable=self.show_labels_var
        )
        self.show_labels_checkbox.pack(pady=5)

        # Plot area
        self.plot_frame = CTK.CTkFrame(self)
        self.plot_frame.pack(side="right", fill="both", expand=True)

        CTK.CTkLabel(control_frame, text="Colors").pack(pady=(10, 0))
        self.color_frame = CTK.CTkFrame(control_frame)
        self.color_frame.pack(pady=5)
        self.color_dropdowns = []

    def load_dataframe(self, df):
        self.df = df
        cols = list(df.columns)

        self.x_dropdown.configure(values=cols)
        if cols:
            self.x_var.set(cols[0])

        self.y_listbox.delete(0, tk.END)
        for col in cols:
            self.y_listbox.insert(tk.END, col)

        self.update_chart()

    def update_chart(self):
        if self.df is None:
            return

        x_col = self.x_var.get()
        y_cols = [self.y_listbox.get(i) for i in self.y_listbox.curselection()]
        if not x_col or not y_cols:
            return

        # --- Rebuild color selectors (one per Y series) ---
        for widget in self.color_frame.winfo_children():
            widget.destroy()

        previous_colors = [
            dd.get() for dd in self.color_dropdowns
        ] if self.color_dropdowns else []

        self.color_dropdowns = []

        for i, col in enumerate(y_cols):
            row = CTK.CTkFrame(self.color_frame)
            row.pack(fill="x", pady=2)

            dd = CTK.CTkComboBox(row, values=list(COMPANY_COLORS.keys()))
            default_color = (
                previous_colors[i]
                if i < len(previous_colors)
                else list(COMPANY_COLORS.keys())[0]
            )
            dd.set(default_color)
            dd.pack(side="left", padx=5)

            preview = CTK.CTkLabel(
                row, text="", width=20, height=20,
                fg_color=COMPANY_COLORS[dd.get()]
            )
            preview.pack(side="left", padx=5)

            def make_update(preview_label):
                return lambda choice: preview_label.configure(
                    fg_color=COMPANY_COLORS[choice]
                )

            dd.configure(command=make_update(preview))
            self.color_dropdowns.append(dd)

        # --- Create line chart ---
        fig, ax = plt.subplots(figsize=(6, 4))

        for i, col in enumerate(y_cols):
            ax.plot(
                self.df[x_col],
                self.df[col],
                label=col,
                color=COMPANY_COLORS[self.color_dropdowns[i].get()]
            )

        ax.set_xlabel(self.x_label_entry.get() or x_col)
        ax.set_ylabel(self.y_label_entry.get() or "Value")
        ax.set_title(self.title_label_entry.get() or "")
        ax.legend()

        # --- Render ---
        if self.canvas:
            self.canvas.get_tk_widget().destroy()

        self.canvas = FigureCanvasTkAgg(fig, master=self.plot_frame)
        self.canvas.draw()
        self.canvas.get_tk_widget().pack(fill="both", expand=True)

class sciotoPlot:
    def __init__(self, root, size):
        self.root = root
        self.root.title('Scioto Analysis Graph Maker')
        self.root.geometry(f'{size[0]}x{size[1]}')
        self.root.minsize(size[0], size[1])
        self.df = None

        self.chart_classes = {
            "Grouped Bar": lambda parent: GroupedBarChartFrame(parent, COMPANY_COLORS),
            "Scatter": lambda parent: ScatterChartFrame(parent, COMPANY_COLORS),
            'Line Chart': lambda parent: LineChartFrame(parent, COMPANY_COLORS)
        }

        # --- Create a top toolbar frame ---
        self.top_frame = CTK.CTkFrame(self.root)
        self.top_frame.grid(row=0, column=0, sticky="ew", padx=10, pady=10)


        # --- Main content area (future plot area) ---
        self.main_frame = CTK.CTkFrame(self.root)
        self.main_frame.grid(row=1, column=0, sticky="nsew")

        # Make columns expand evenly
        self.top_frame.grid_columnconfigure((0, 1, 2, 3), weight=1)

        # --- Add widgets to the toolbar ---
        self.csv_button = CTK.CTkButton(
            self.top_frame,
            text="Load CSV",
            command=self.load_csv
        )
        self.csv_button.grid(column=2, row=0, padx=10, pady=10)

        self.download_button_frame = ButtonFrame(
            self.top_frame,
            text="Download Plot",
            callback=self.download_grouped_bar_plot
        )
        self.download_button_frame.grid(column=4, row=0, padx=10, pady=10)

        self.chart_selector = CTK.CTkComboBox(
            self.top_frame,
            values=list(self.chart_classes.keys()),
            command=self.switch_chart
        )
        self.chart_selector.grid(column=1, row=0, padx=10, pady=10)

        self.current_chart = None
        self.chart_selector.set("Grouped Bar")
        self.switch_chart("Grouped Bar")

        # Allow the plot to expand
        self.main_frame.grid_rowconfigure(0, weight=1)
        self.main_frame.grid_columnconfigure(0, weight=1)

        # Allow main content to expand
        self.root.grid_rowconfigure(1, weight=1)
        self.root.grid_columnconfigure(0, weight=1)

    def download_grouped_bar_plot(self):
        fig = self.current_chart.get_figure()
        if fig is None:
            print("No plot available to save.")
            return

        file_path = filedialog.asksaveasfilename(
            defaultextension=".png",
            filetypes=[
                ("PNG Image", "*.png"),
                ("JPEG Image", "*.jpg"),
                ("PDF Document", "*.pdf"),
                ("All Files", "*.*")
            ]
        )

        if not file_path:
            return

        fig.savefig(file_path, dpi=300, bbox_inches="tight")
        print(f"Plot saved to: {file_path}")

    def switch_chart(self, chart_name):
        if self.current_chart:
            self.current_chart.destroy()

        ChartClass = self.chart_classes[chart_name]
        self.current_chart = ChartClass(self.main_frame)
        self.current_chart.grid(row=0, column=0, sticky="nsew")

        # If data already loaded, push it into the new chart
        if self.df is not None:
            self.current_chart.load_dataframe(self.df)

    def load_csv(self):
        file_path = filedialog.askopenfilename(
            filetypes=[("CSV Files", "*.csv"), ("All Files", "*.*")]
        )
        if not file_path:
            return

        # Load once
        self.df = pd.read_csv(file_path)

        # Push data into the active chart
        if self.current_chart:
            self.current_chart.load_dataframe(self.df)


size = [800,800]

root = CTK.CTk()
app = sciotoPlot(root, size)
root.mainloop()
