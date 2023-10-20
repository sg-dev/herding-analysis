import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


BASE_FIG_UNIT = 2.5


def calculate_cooperation_proportions(df):
    before = df[df["Round"] < 6].groupby("Participant id")["Decision"].mean()
    after = df[df["Round"] > 6].groupby("Participant id")["Decision"].mean()
    df["prop_coop_before"] = df["Participant id"].map(before)
    df["prop_coop_after"] = df["Participant id"].map(after)
    return df


def classify(row):
    if row["prop_coop_before"] < 0.5 and row["prop_coop_after"] > 0.5:
        return "pure herding"
    elif row["prop_coop_before"] > 0.5 and row["prop_coop_after"] < 0.5:
        return "pure rational"
    elif row["prop_coop_before"] < row["prop_coop_after"]:
        return "relative herding"
    elif row["prop_coop_before"] > row["prop_coop_after"]:
        return "relative rational"
    else:
        return "no change"


def load_data():
    panel_df = pd.read_csv("../data/processed/panel_data.csv")
    panel_df = calculate_cooperation_proportions(panel_df)

    panel_df["classification"] = panel_df.apply(classify, axis=1)
    panel_df["rel_classification"] = panel_df["classification"].replace(
        {
            "pure herding": "herding",
            "pure rational": "rational",
            "relative herding": "herding",
            "relative rational": "rational",
            "no change": "no change",
        }
    )

    # "debrief.1.player.debrief" is a factor with 3 levels.  # Convert it to a factor
    panel_df["debrief_self"] = panel_df["debrief.1.player.debrief"].astype("category")
    panel_df["debrief_other"] = panel_df["debrief.1.player.debrief2"].astype("category")

    return panel_df


def get_classification_counts(panel_df):
    c_counts = panel_df.groupby("classification")["Participant id"].nunique()
    rel_c_counts = panel_df.groupby("rel_classification")["Participant id"].nunique()
    return c_counts, rel_c_counts


def plot_hard_switch(ax=None):
    # add a horizontal line at 1 from 0 to prop_coop 0.5
    if ax is None:
        ax = plt.gca()
    ax.axhline(1, xmax=0.495, color="gray", linestyle="-")
    ax.axhline(0, xmin=0.495, color="gray", linestyle="-")
    # add a vertical line at 0.5
    ax.axvline(0.495, ymin=0.09, ymax=0.95, color="gray", linestyle="-", label="Perfect Rat.")

def plot_sigmoid(ax=None):
    if ax is None:
        ax = plt.gca()
    def sigmoid(x):
        return 1 / (1 + np.exp((x-0.5)*10))

    x = np.linspace(0, 1, 100)  # Generate 100 points between 0 and 1
    y = sigmoid(x)
    ax.plot(x, y, color="lightgray", linestyle="--", label="Bounded Rat.")


def plot_decision_with_ci(
    df: pd.DataFrame,
    title: str | None,
    ax=None,
    color="black",
    label=None,
    add_hard_switch=False,
):
    """
    Function to plot the mean decision with confidence intervals.

    Args:
        df: pandas.DataFrame that contains the data
        ax: matplotlib axes object to plot on
        color: color of the plot line
        label: label of the plot line
        add_hard_switch: boolean that indicates whether to add a hard switch to the plot

    Returns:
        None
    """
    # Calculate the 99% confidence interval
    Z_99 = 2.576
    mean = df.groupby("prop_coop")["Decision"].mean()
    sem = df.groupby("prop_coop")["Decision"].sem() * Z_99

    # Create a new axes object if none is provided
    if ax is None:
        ax = plt.gca()

    # Plot the mean with error bars representing the standard error of mean
    mean.plot(yerr=sem, color=color, ax=ax, label=label)

    if add_hard_switch:
        plot_hard_switch(ax)
        plot_sigmoid(ax)
    # else:
    ax.axhline(0.5, color="gray", linestyle="--", lw=0.5)
    ax.axvline(0.495, ymin=0.09, ymax=0.95, color="gray", linestyle="--", lw=0.5)

    # Set the y-axis limits
    ax.set_ylim(-0.1, 1.05)

    # Set the ticks on the y and x axes
    ax.set_yticks([0, 0.5, 1])
    ax.set_xticks([0.1, 0.5, 0.9])

    # Add a hard switch to the plot if indicated

    # Set the labels for the y and x axes
    ax.set_ylabel("P(Cooperate)")
    ax.set_xlabel("Proportion of cooperators in the group")
    if title is not None:
        ax.set_title(title.title())


def plot_decision_grid(
    panel_df,
    classifications: list,
    counts,
    include_bots: bool,
    classification_key="classification",
    add_hard_switch=False,
):

    n_c = len(classifications)
    if n_c== 4:
        fig, axs = plt.subplots(
            2,
            2,
            figsize=(2 * BASE_FIG_UNIT, 2 * BASE_FIG_UNIT),
            sharex=True,
            sharey=True,
        )
    elif n_c < 4:
        fig, axs = plt.subplots(
            1, n_c, figsize=(n_c*BASE_FIG_UNIT, BASE_FIG_UNIT), sharex=True, sharey=True
        )
    elif n_c == 4:
        fig, axs = plt.subplots(
            2, 2, figsize=(2 * BASE_FIG_UNIT, 2 * BASE_FIG_UNIT), sharex=True, sharey=True
        )
    else:
        raise ValueError("Only 2 or 4 classifications are supported.")

    for i, classification in enumerate(classifications):
        try:
            current_ax = axs[i // len(axs[0]), i % len(axs[0])]
        except TypeError:
            current_ax = axs[i]

        data_subset = panel_df[panel_df[classification_key] == classification]

        if include_bots:
            panel_bots = data_subset[data_subset["participant.in_deception"] == False]
            panel_humans = data_subset[data_subset["participant.in_deception"] == True]
            plot_decision_with_ci(
                panel_bots,
                classification,
                ax=current_ax,
                color="darkorange",
                label="No deception (Bots)",
                add_hard_switch=add_hard_switch,
            )
            plot_decision_with_ci(
                panel_humans,
                classification,
                ax=current_ax,
                color="darkblue",
                label="Deception (Players)",
                add_hard_switch=add_hard_switch,
            )
            if i == 0:
                current_ax.legend(loc="lower right")
        else:
            plot_decision_with_ci(
                data_subset,
                classification,
                ax=current_ax,
                color="black",
                add_hard_switch=add_hard_switch,
            )

        n = counts[classification]
        count_and_pct = f"n = {n} ({n / counts.sum() * 100:.0f}%)"
        current_ax.text(
            0.05,
            0.95,
            count_and_pct,
            transform=current_ax.transAxes,
            verticalalignment="top",
        )


panel_df = load_data()
panel_df.to_csv("../data/processed/panel_data_classification.csv", index=False)
classification_counts, rel_classification_counts = get_classification_counts(panel_df)

classifications = [
    "pure herding",
    "pure rational",
    "relative herding",
    "relative rational",
]
rel_classification = ["herding", "rational", "no change"]

plt.style.use(["nature", "no-latex"])

###### PLOTS ######

# First Plot: Proportion of cooperation over time
fig, ax = plt.subplots(figsize=(BASE_FIG_UNIT, BASE_FIG_UNIT))
plot_decision_with_ci(panel_df, title=None, ax=ax, color="black", label="Observed", add_hard_switch=True)
ax.scatter(0.495, 0.5, color="red", marker="o", s=20, zorder=2)
# add legend
ax.legend(loc="lower left", frameon=False, fontsize=6)
plt.savefig("../reports/figures/prop_coop.pdf", bbox_inches="tight")


# Second Plot: Proportion of cooperation over time, split by classification
plot_decision_grid(panel_df, classifications, classification_counts, include_bots=False)
plt.savefig("../reports/figures/prop_coop_by_classification.pdf", bbox_inches="tight")

plot_decision_grid(panel_df, classifications, classification_counts, include_bots=True)
plt.savefig(
    "../reports/figures/prop_coop_by_classification_with_bots.pdf", bbox_inches="tight"
)

# Third Plot: Proportion of cooperation over time, split by relative classification
plot_decision_grid(
    panel_df,
    rel_classification,
    rel_classification_counts,
    include_bots=False,
    classification_key="rel_classification",
)
plt.savefig(
    "../reports/figures/prop_coop_by_rel_classification.pdf", bbox_inches="tight"
)

plot_decision_grid(
    panel_df,
    rel_classification,
    rel_classification_counts,
    include_bots=True,
    classification_key="rel_classification",
)
plt.savefig(
    "../reports/figures/prop_coop_by_rel_classification_with_bots.pdf",
    bbox_inches="tight",
)


# # -----------------------------------------------------------------------
def plot_answer_speed(df, classification, ax):
    mean = (
        df[df["classification"] == classification].groupby("Round")["TimeSpent"].mean()
        / 10
    )
    median = (
        df[df["classification"] == classification]
        .groupby("Round")["TimeSpent"]
        .median()
        / 10
    )

    ax.plot(mean.index, mean, color="black", lw=1.5, label="Mean")
    ax.plot(median.index, median, color="black", lw=1.5, linestyle="--", label="Median")


# plot TimeSpent by classification in grid
fig, axs = plt.subplots(2, 3, figsize=(7.5, 5), sharex=True, sharey=True)
for i, classification in enumerate(
    [
        "pure herding",
        "pure rational",
        "relative herding",
        "relative rational",
        "no change",
    ]
):
    current_axs = axs[i // 3, i % 3]
    plot_answer_speed(panel_df, classification, current_axs)
    current_axs.set_title(classification.title())

    current_axs.set_xlabel("Round")
    current_axs.set_ylabel("Time spent (s)")

    if i == 0:  # print legend only once
        current_axs.legend(loc="upper right")
        # remove legend bounding box
        current_axs.get_legend().get_frame().set_linewidth(0.0)

plt.tight_layout()
plt.savefig("../reports/figures/time_spent_by_classification.pdf", bbox_inches="tight")




# # break down the debriefing by classification
# debriefing_counts = (
#     panel_df.groupby(["classification", "debrief_self"])["Participant id"]
#     .nunique()
#     .unstack()
# )
# debriefing_counts = debriefing_counts.reindex(
#     ["pure herding", "pure rational", "relative herding", "relative rational"]
# )

# debriefing_counts = (
#     panel_df.groupby(["classification", "debrief_other"])["Participant id"]
#     .nunique()
#     .unstack()
# )
# debriefing_counts = debriefing_counts.reindex(
#     ["pure herding", "pure rational", "relative herding", "relative rational"]
# )

# debrieding_pct = debriefing_counts / debriefing_counts.sum().sum() * 100
# # add row and column totals
# debrieding_pct.loc["Total"] = debrieding_pct.sum()
# debrieding_pct["Total"] = debrieding_pct.sum(axis=1)
# # round to 1 decimal
# debrieding_pct = debrieding_pct.round(1)
# debrieding_pct


# debriefing_counts_other = panel_df.groupby(["rel_classification", "debrief_other"])[
#     "Participant id"
# ].nunique()
# debriefing_counts_self = panel_df.groupby(["rel_classification", "debrief_self"])[
#     "Participant id"
# ].nunique()

# # join in one dataframe
# debriefing_counts = pd.concat([debriefing_counts_other, debriefing_counts_self], axis=1)
# debriefing_counts.columns = ["Other", "Self"]

# # debriefing_counts.reset_index(level=1, inplace=True)
# # aggregate "sophisticated", "random" and "only_d" into "other" in column "level_1"
# # debriefing_counts['level_1'].map({'sophisticated': 'other', 'random': 'other', 'only_d': 'other'},

# # debriefing_counts.unstack().T.plot.bar(figsize=(6, 4))

# with plt.style.context("nature", "no-latex"):
#     # consider only "pure" classifications
#     fig, axs = plt.subplots(1, 2, figsize=(7, 3), sharex=True, sharey=True)

#     debriefing_counts = debriefing_counts.loc[["herding", "rational"]]

#     # only self
#     debriefing_counts["Self"].unstack().T.plot.barh(ax=axs[0])
#     axs[0].set_title("Ego")

#     debriefing_counts["Other"].unstack().T.plot.barh(ax=axs[1])
#     axs[1].set_title("Alter")

#     plt.show()


# debr_pct = debriefing_counts.loc["herding"] / debriefing_counts.loc["herding"].sum()
# (debr_pct * 100).sort_values("Self").plot.barh()
# plt.title("Herding")
# plt.show()

# debr_pct = debriefing_counts.loc["rational"] / debriefing_counts.loc["rational"].sum()
# (debr_pct * 100).sort_values("Self").plot.barh()
# plt.title("Rational")
# # hide legend
# plt.show()


# df_fixed = pd.read_csv("../data/processed/panel_data_fixed_effects.csv")

# df_fixed

# # count boxes collected by classification
# (
#     panel_df.groupby("classification")["bret.1.player.boxes_collected"].mean() - 50
# ).plot.bar()
# plt.show()


# def crra(k, max_box=102):
#     r = np.log((max_box - k) / (max_box - 1 - k) / np.log((k + 1) / k))
#     return r


# panel_df["bret.1.player.boxes_collected"].max()

# panel_df["crra"] = panel_df["bret.1.player.boxes_collected"].apply(crra)
# panel_df.to_csv("../data/processed/panel_data_classification.csv", index=False)

# import numpy as np

# plt.plot(crra(np.arange(1, 99)))
# plt.show()
