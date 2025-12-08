# StaggR: The Staggered Protocol Scheduling Optimizer 🧪

[](https://www.r-project.org/)
[](https://shiny.posit.co/)

**StaggR** is an interactive web application designed to eliminate the headache of scheduling complex, time-sensitive laboratory experiments. For any protocol that requires processing multiple samples through the same series of timed steps, StaggR provides the tools to design, visualize, optimize, and execute the workflow with precision and confidence.

It is built for researchers in molecular biology, genomics, biochemistry, and any other field where timing is critical. It solves the difficult problem of determining the optimal "staggering" interval between samples to maximize throughput without creating conflicts where you'd need to be in two places at once.

-----


## Key Features

  * **Interactive Gantt Chart:** Visualize your entire experimental timeline, with each sample and every step clearly laid out.
  * **Automatic Scheduling:** Automatically calculates the fastest possible, conflict-free staggering interval for your protocol, taking into account hands-on time and necessary buffers.
  * **Manual Simulation:** Test any staggering interval to see how it impacts your workflow, identify potential bottlenecks, and plan for breaks.
  * **Live "Cockpit" View:** Run your experiment with a real-time display that tells you exactly what to do, for which sample, and when. It includes countdown timers for both hands-on tasks and wait times.
  * **Time Course Helper:** Rapidly generate complex experimental arms by converting a simple list of time points (e.g., "5, 15, 30, 60 min") into a series of fully timed steps.
  * **Full Reproducibility:** Save and load complete experimental sessions as `.json` files, capturing every parameter from step durations to color schemes.
  * **Exportable Results:** Download your Gantt chart as a high-quality PDF or PNG, and export detailed schedules for your electronic lab notebook.

-----


## Getting Started

You can run StaggR on your own computer with R and RStudio. The application is designed using `renv` to ensure that all required packages and their exact versions are installed automatically, guaranteeing it will run correctly.

### **Prerequisites**

  * **R:** Version 4.0 or newer.
  * **RStudio:** Recommended for the best experience.

### **Installation & Launch**

1.  **Clone the Repository:** Clone this project from its GitHub repository to your local machine.
2.  **Open the Project:** Open the `StaggR.Rproj` file in RStudio.
3.  **Restore the Environment:** The first time you open the project, the `renv` package should automatically prompt you to restore the project's environment. Enter `renv::restore()` in the console and press `Enter`. This will install all the necessary packages as defined in the `renv.lock` file.
4.  **Run the App:** Once the packages are restored, you can launch the application by running the following command in the RStudio console:
    ```r
    shiny::runApp('app.R')
    ```

-----


## How to Use the App

The application is organized into a sidebar for inputs and a main panel for outputs. Here is a detailed walkthrough of each component.

### **Part 1: Defining Your Protocol (Sidebar)**

This is where you tell StaggR about your experiment.

  * **Experiment Parameters:**
      * **Protocol Name:** Give your experiment a descriptive name (e.g., "dTAG-13 Degron Time Course"). This name will be used on plots and in exported filenames.
      * **Number of Samples & Custom Names:** Enter the total number of samples you will be processing. The text box below will auto-populate with default names, which you can edit. The names should be comma-separated. The number and the names are synced automatically.
  * **Step Definitions:**
      * **Number of Steps:** Specify how many distinct, hands-on steps are in your protocol. The inputs below will update dynamically.
      * For each step, you must define:
          * `Step Name`: A short, clear name (e.g., "Add 4sU Label", "Quench Reaction").
          * `Duration`: The amount of "hands-on" time required to complete that step for a single sample.
          * `Unit`: The time unit for the duration (sec, min, or hr).
          * `Color`: A color to represent this step on the Gantt chart.
  * **Time Between Steps:**
      * This section defines the "wait" or "incubation" times. The value for a given row is the time that must pass between the **end** of that step and the **start** of the *next* step. These are your incubations, centrifugations, or other automated/waiting periods.

### **Part 2: Scheduling & Plot Settings**

This section controls how the schedule is calculated and displayed.

  * **Optimization Mode:**
      * `Automatic`: This is the most powerful feature. StaggR will automatically find the shortest possible staggering interval that results in zero conflicts.
      * `Manual interval`: This mode lets you simulate a specific interval. It's useful for seeing how a chosen interval (e.g., "start a new sample every 10 minutes") would play out and where conflicts might arise.
  * **Buffer Time:** This is a crucial parameter for realistic scheduling. It's a small, fixed amount of time added to the end of every hands-on task to account for the "human factor"—moving between samples, changing pipette tips, etc. This buffer is used for conflict detection but is visualized separately from the main task.
  * **Optimizer Granularity:** This advanced setting controls the precision of the `Automatic` mode. A finer granularity (e.g., 1 second) is more precise but takes longer to compute, whereas a coarser granularity (e.g., 1 minute) is faster but might slightly overestimate the optimal interval.

### **Part 3: Generating and Viewing the Schedule**

Once your parameters are set, click the **"Generate Schedule"** button. The main panel will populate with the results.

  * **Gantt Chart Tab:** This provides a complete visual overview.
      * Each sample has its own horizontal track.
      * The bottom track, **"Hands-On Time"**, is an aggregate of all your hands-on work. This is where you look for conflicts.
          * **Busy (Black):** A time block where you are actively working on one sample.
          * **Buffer (Gray):** The "human factor" time you allocated.
          * **Overbooked (Red):** A conflict\! This indicates a time block where the schedule requires you to perform more than one task simultaneously. If you see red, your schedule is not feasible.
  * **Schedule by Time Tab:** This is a chronological, step-by-step list of your entire experiment. It tells you the exact time, sample, and step for every action. This is the table you'll likely follow during execution.
  * **Schedule by Sample Tab:** This table shows the start time of every step, organized by sample. It's useful for quickly seeing, for example, when "Sample 3" needs to be harvested.

### **Part 4: Running a Live Experiment**

Navigate to the **"Live Time Course"** tab after generating a valid schedule.

  * **Controls:** Use the `Start`, `Pause`, and `Stop/Reset` buttons to control the timer.
  * **Cockpit Display:** This central panel is your guide.
      * A large **countdown timer** shows the time remaining for the current event (either a hands-on step or a wait period).
      * **Cards** for "Previous Step," "Current Step," and "Next Step" provide context, telling you what you just did, what you're doing now (or waiting for), and what's coming up next.
      * While the timer is running, the **"Schedule by Time"** table will automatically highlight the currently active row, keeping you oriented.

### **Part 5: Saving & Loading Your Work**

Reproducibility is key.

  * **Loading a Session:** Use the **"Upload Session"** button on the sidebar. You can upload a full `.json` session file, which will restore every single parameter in the app exactly as you saved it.
  * **Saving Your Work:** The best way to save a complete, reproducible session is to generate a report.
      * On the **"Gantt Chart"** tab, click **"Download Full Report (.zip)"**.
      * This will create a zip file containing the Gantt chart, all schedule tables, and a `protocol_parameters.csv` file. You can archive this `.zip` file. In a future session, you can use the `.csv` from this report to quickly load your protocol steps.

-----


## Built-in Examples 

The **"About"** tab contains buttons to load pre-configured example protocols relevant to research in transcription biology and genomics. Click any of these to see how a complex protocol is set up in StaggR.

  * **Fixation Time Course (e.g., for ChIP):** A protocol involving a time-sensitive fixation and quenching reaction across many samples.
  * **Degron Time Course:** A simple protocol for a protein degradation experiment, perfect for use with the "Time Course Helper".
  * **Metabolic RNA Labeling (e.g., 4sU/4tU/BrdU):** A multi-step protocol involving labeling, cell lysis, and purification steps.

