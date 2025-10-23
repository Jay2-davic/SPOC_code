#%%
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm
import math
from scipy.optimize import minimize
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from scipy import special  # Add this import for erf function
import os
import yaml
import sys

# ---- CONFIGURABLE ----
CONTINUOUS_FEATURES = ['Additive', 'Salt']
MATERIALS = ['LiTFSI', 'NaTFSI']
PRINT_TYPE = 'PCB'
BOUNDS = [(0.0, 15.0), (0.0, 30.0)]
N_RANDOM_STARTS = 500
N_ESTIMATORS = 100
ACQUISITION = 'PI'
# ----------------------

# Simple config utility function
def load_configs(config_path):
    """Load configuration from YAML file"""
    if os.path.exists(config_path):
        with open(config_path, 'r') as f:
            return yaml.safe_load(f)
    return {}

def load_data(csv_path):
    df = pd.read_csv(csv_path)
    # Map categorical/material columns to one-hot
    for mat in MATERIALS:
        df[f'material_{mat}'] = (df['material'] == mat).astype(int)  # lowercase
    df['print_type_PCB'] = (df['print_type'] == PRINT_TYPE).astype(int)  # lowercase
    features = ['fcomp_additive_wt_pct', 'fcomp_salt_wt_pct'] + [f'material_{m}' for m in MATERIALS] + ['print_type_PCB']
    X = df[features].values
    y = df['ionic_conductivity_final'].values
    return X, y, features, df

def build_model(input_dim):
    """Build a Random Forest model with specified parameters"""
    return RandomForestRegressor(n_estimators=N_ESTIMATORS, n_jobs=-1)

def get_mean_std(model, X):
    preds = np.array([tree.predict(X) for tree in model.estimators_])
    mean = np.mean(preds, axis=0)
    std = np.std(preds, axis=0)
    std = np.clip(std, 1e-12, None)  # avoid zero std
    return mean, std

def probability_improvement(mean, std, y_best):
    """Probability of Improvement acquisition function"""
    # Handle the case when std is 0
    with np.errstate(divide='ignore', invalid='ignore'):
        # Since we want to maximize conductivity (which is negative), we need to flip the sign
        z = (mean - y_best) / std  # Changed from (y_best - mean)
        pi = 0.5 * (1 + np.vectorize(lambda x: np.nan if np.isnan(x) else special.erf(x / np.sqrt(2)))(z))
    return pi

def expected_improvement(mean, std, y_best):
    """Expected Improvement acquisition function"""
    # Handle the case when std is 0
    with np.errstate(divide='ignore', invalid='ignore'):
        # Since we want to maximize conductivity (which is negative), we need to flip the sign
        z = (mean - y_best) / std  # Changed from (y_best - mean)
        ei = (mean - y_best) * 0.5 * (1 + np.vectorize(lambda x: np.nan if np.isnan(x) else special.erf(x / np.sqrt(2)))(z))
        ei += std * np.vectorize(lambda x: 0 if np.isnan(x) or x < -10 else np.exp(-0.5 * x * x) / np.sqrt(2 * np.pi))(z)
    return ei

def objective(x_cont, mat_flags, y_best, return_acq=True):
    x_full = np.hstack([x_cont, mat_flags, [1]]).reshape(1, -1)
    x_full = scaler.transform(x_full)
    mean, std = get_mean_std(model, x_full)

    if return_acq:
        # negate for optimizer to maximize acquisition
        acq_val = acquisition(mean, std, y_best=y_best)
        return -acq_val[0] if isinstance(acq_val, np.ndarray) else -acq_val
    else:
        # maximize predicted conductivity
        return -mean[0]

def recommend_by_target_prediction(X, y, scaler, model, acquisition):
    """Recommend parameters by directly maximizing the predicted target value"""
    return_acq = False
    bounds_cont = BOUNDS
    best = {'material': None, 'x_cont': None, 'score': np.inf}
    material_opts = np.array([[1, 0], [0, 1]])  # [LiTFSI, NaTFSI]

    for mat_flags in material_opts:
        for x0 in np.random.uniform([b[0] for b in bounds_cont],
                                    [b[1] for b in bounds_cont],
                                    size=(N_RANDOM_STARTS, 2)):
                res = minimize(lambda x: objective(x, mat_flags, y_best=np.max(y), return_acq=False),
                x0=x0, bounds=bounds_cont)
                if res.fun < best['score']:
                    best['score'] = res.fun
                    best['x_cont'] = res.x
                    best['material'] = mat_flags

    suggested_cont = best['x_cont']
    suggested_cont_rounded = np.round(suggested_cont, 1)
    suggested_material = 'LiTFSI' if best['material'][0] == 1 else 'NaTFSI'

    df_suggestion = pd.DataFrame({
        'Parameter': ['Additive Weight (%)', 'Salt Weight (%)', 'Material', 'Print Type (PCB)'],
        'Suggested Continuous': [suggested_cont[0], suggested_cont[1], suggested_material, 1],
        'Suggested for Experiment': [suggested_cont_rounded[0], suggested_cont_rounded[1], suggested_material, 1]
    })

    return df_suggestion, best

def recommend_by_acquisition(X, y, scaler, model, acquisition):
    """Recommend parameters by maximizing the acquisition function"""
    return_acq = True
    bounds_cont = BOUNDS
    best = {'material': None, 'x_cont': None, 'score': np.inf}
    material_opts = np.array([[1, 0], [0, 1]])  # [LiTFSI, NaTFSI]

    for mat_flags in material_opts:
        for x0 in np.random.uniform([b[0] for b in bounds_cont],
                                    [b[1] for b in bounds_cont],
                                    size=(N_RANDOM_STARTS, 2)):
                res = minimize(lambda x: objective(x, mat_flags, y_best=np.max(y), return_acq=True),
                            x0=x0, bounds=bounds_cont)
                if res.fun < best['score']:
                    best['score'] = res.fun
                    best['x_cont'] = res.x
                    best['material'] = mat_flags

    # 최종 추천 조성
    suggested_cont = best['x_cont']
    suggested_cont_rounded = np.round(suggested_cont, 1)
    suggested_material = 'LiTFSI' if best['material'][0] == 1 else 'NaTFSI'

    df_suggestion = pd.DataFrame({
        'Parameter': ['Additive Weight (%)', 'Salt Weight (%)', 'Material', 'Print Type (PCB)'],
        'Suggested Continuous': [suggested_cont[0], suggested_cont[1], suggested_material, 1],
        'Suggested for Experiment': [suggested_cont_rounded[0], suggested_cont_rounded[1], suggested_material, 1]
    })

    return df_suggestion, best

def plot_target_prediction_heatmap(X, y, scaler, model, suggested_cont, mat_flags):
    """
    Target prediction heatmap styled to match plot_recommendations_comparison.
    Explicit LogNorm bounds:
      - vmax = 10**floor(log10(max_data))
      - vmin = 10**ceil(log10(min_positive_data))
    No suggested points. Scientific notation on colorbar. Increased text sizes.
    """
    import numpy as np
    import matplotlib.pyplot as plt
    from matplotlib.colors import LogNorm

    # Grid generation
    add_vals = np.linspace(0, 15, 500)
    salt_vals = np.linspace(0, 30, 500)
    grid = np.array([[a, s] for a in add_vals for s in salt_vals])

    print_type_flag = 1
    x_grid = np.hstack([
        grid,
        np.tile(mat_flags, (grid.shape[0], 1)),
        np.ones((grid.shape[0], 1)) * print_type_flag
    ])

    x_grid = scaler.transform(x_grid)
    mean, std = get_mean_std(model, x_grid)

    # Match original style: display -mean
    Z = (-mean).reshape(len(add_vals), len(salt_vals))

    # For display, ensure positivity for LogNorm following original behavior
    Z_display = -Z  # positive values expected for log scaling

    # Prepare positive finite values for computing bounds
    Z_pos = Z_display[np.isfinite(Z_display) & (Z_display > 0)]
    if Z_pos.size == 0:
        # Fabricate small positive data to avoid crash, keep plot deterministic
        Z_pos = np.array([1e-6], dtype=float)

    data_min = float(np.min(Z_pos))
    data_max = float(np.max(Z_pos))

    # Log-space floor and ceil
    log_min = np.log10(data_min)
    log_max = np.log10(data_max) + 1

    # vmin = 10**ceil(log10(min)), vmax = 10**floor(log10(max))
    vmin = 10.0 ** np.ceil(log_min)
    vmax = 10.0 ** np.floor(log_max)

    # Safety adjustments if range is invalid or degenerate
    if not np.isfinite(vmin) or vmin <= 0:
        vmin = 1e-8
    if not np.isfinite(vmax) or vmax <= 0:
        vmax = max(1.0, data_max)
    if vmin >= vmax:
        # Widen to a decade around data_max
        vmax = 10.0 ** np.floor(log_max)
        vmin = vmax / 10.0
        if vmin <= 0 or vmin >= vmax:
            vmin = max(1e-12, data_min / 10.0)
            vmax = max(vmin * 10.0, data_max)

    # Clip display data to [vmin, vmax], replace nonpositive or nonfinite with vmin
    Z_disp_clipped = np.array(Z_display, dtype=float)
    Z_disp_clipped[~np.isfinite(Z_disp_clipped)] = vmin
    Z_disp_clipped[Z_disp_clipped <= 0] = vmin
    Z_disp_clipped = np.clip(Z_disp_clipped, vmin, vmax)

    # Build LogNorm
    norm = LogNorm(vmin=vmin, vmax=vmax)

    fig, ax = plt.subplots(figsize=(10, 8))
    im = ax.imshow(
        Z_disp_clipped,
        origin='lower',
        extent=[salt_vals.min(), salt_vals.max(), add_vals.min(), add_vals.max()],
        aspect='auto',
        norm=norm
    )

    # No suggested points, no legend
    ax.set_xlabel('Salt Weight (%)', fontsize=14)
    ax.set_ylabel('Additive Weight (%)', fontsize=14)
    ax.set_title('Ionic Conductivity Heatmap', fontsize=16)
    ax.tick_params(axis='both', which='major', labelsize=12)

    cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    # Colorbar ticks at decades within [vmin, vmax]
    tick_decades = np.arange(np.ceil(np.log10(vmin)), np.floor(np.log10(vmax)) + 1)
    if tick_decades.size == 0:
        tick_locations = np.geomspace(vmin, vmax, 3)
    else:
        tick_locations = 10.0 ** tick_decades
    cbar.set_ticks(tick_locations)
    cbar.set_ticklabels([f"{x:.1e}" for x in tick_locations])
    cbar.set_label('Ionic Conductivity', fontsize=14)
    cbar.ax.tick_params(labelsize=12)

    plt.tight_layout()
    plt.show()

    # Promising region analysis unchanged
    threshold = 0.9 * Z.max()
    mask = Z >= threshold
    if np.any(mask):
        rows, cols = np.where(mask)
        add_min, add_max = add_vals[rows.min()], add_vals[rows.max()]
        salt_min, salt_max = salt_vals[cols.min()], salt_vals[cols.max()]
        import pandas as pd
        df_region = pd.DataFrame({
            'Variable': ['Additive Weight (%)', 'Salt Weight (%)'],
            'Min': [add_min, salt_min],
            'Max': [add_max, salt_max]
        })
        print("\nPromising Region (>90% of max performance):")
        print(df_region)

def plot_acquisition_heatmap(X, y, scaler, model, acquisition, suggested_cont, mat_flags):
    print_type_flag = 1

    add_vals = np.linspace(0, 15, 500)
    salt_vals = np.linspace(0, 30, 500)
    grid = np.array([[a, s] for a in add_vals for s in salt_vals])

    x_grid = np.hstack([
        grid,
        np.tile(mat_flags, (grid.shape[0], 1)),
        np.ones((grid.shape[0], 1)) * print_type_flag
    ])

    x_grid = scaler.transform(x_grid)
    mean, std = get_mean_std(model, x_grid)

    y_best = float(np.max(y))
    acq = acquisition(mean, std, y_best)

    Z = acq.reshape(len(add_vals), len(salt_vals))
    eps = 1e-12
    Z_pos = np.clip(Z, eps, None)

    # Calculate vmin and vmax for color scaling (leave as data-driven)
    vmin = float(Z_pos.min())
    data_max = float(Z_pos.max())
    log_min = np.floor(np.log10(vmin))
    log_max = np.ceil(np.log10(data_max))
    vmax = 10 ** log_max  # or data_max if you want exact data max

    fig, ax = plt.subplots()
    im = ax.imshow(
        Z_pos,
        origin='lower',
        extent=[salt_vals.min(), salt_vals.max(), add_vals.min(), add_vals.max()],
        aspect='auto',
        norm=LogNorm(vmin=vmin, vmax=vmax),
        cmap='viridis'
    )

    best_add, best_salt = suggested_cont
    ax.scatter(best_salt, best_add, color='red', marker='X', s=100, label='Recommended')
    ax.legend(loc='upper right')

    ax.set_xlabel('Salt Weight (%)')
    ax.set_ylabel('Additive Weight (%)')
    ax.set_title('Acquisition Function (PI) Heatmap for ' + ('LiTFSI' if mat_flags[0] == 1 else 'NaTFSI'))

    # Set colorbar ticks for all decades from vmin up to your desired upper limit
    tick_decades = np.arange(log_min, log_max + 2)  # +2 for two decades above
    tick_locations = 10 ** tick_decades
    cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)

    # Set ticks and labels (even if some are above vmax)
    cbar.set_ticks(tick_locations)
    cbar.set_ticklabels([f"{x:.1e}" for x in tick_locations])
    cbar.set_label('PI (log scale)')

    plt.show()

def generate_experiment_points(df_target, df_acq, num_points=8):
    """
    Generate experiment points that pass through both recommendation points.
    Returns the experiment dataframe and the syringe_info dictionary.
    """
    import numpy as np
    import pandas as pd

    # Extract recommendation points
    target_add = df_target.loc[df_target['Parameter'] == 'Additive Weight (%)', 'Suggested for Experiment'].values[0]
    target_salt = df_target.loc[df_target['Parameter'] == 'Salt Weight (%)', 'Suggested for Experiment'].values[0]
    
    acq_add = df_acq.loc[df_acq['Parameter'] == 'Additive Weight (%)', 'Suggested for Experiment'].values[0]
    acq_salt = df_acq.loc[df_acq['Parameter'] == 'Salt Weight (%)', 'Suggested for Experiment'].values[0]
    
    # Create a path that includes both points
    # First, determine the range of points
    add_min = min(target_add, acq_add) - 2.0  # Start 2% below the minimum
    add_max = max(target_add, acq_add) + 0.6  # End 0.6% above the maximum

    # Clamp additive bounds to [0, 15]
    ADD_MIN_BOUND, ADD_MAX_BOUND = 0.0, 15.0
    add_min = max(ADD_MIN_BOUND, add_min)
    add_max = min(ADD_MAX_BOUND, add_max)

    # Edge case, if min surpasses max after clamping, center around the closer point
    if add_min > add_max:
        center = np.clip((target_add + acq_add) / 2.0, ADD_MIN_BOUND, ADD_MAX_BOUND)
        half_span = 0.5  # small span to ensure num_points generation
        add_min = max(ADD_MIN_BOUND, center - half_span)
        add_max = min(ADD_MAX_BOUND, center + half_span)

    # Calculate the slope between the two recommendation points
    if abs(target_add - acq_add) < 0.01:  # If the additive values are very close, vertical line
        additive_values = np.linspace(add_min, add_max, num_points)
        salt_values = np.ones_like(additive_values) * ((target_salt + acq_salt) / 2)
    elif abs(target_salt - acq_salt) < 0.01:  # If the salt values are very close, horizontal line
        additive_values = np.linspace(add_min, add_max, num_points)
        salt_values = np.ones_like(additive_values) * target_salt
    else:
        # Line through both points
        slope = (acq_salt - target_salt) / (acq_add - target_add)
        intercept = target_salt - slope * target_add
        additive_values = np.linspace(add_min, add_max, num_points)
        salt_values = slope * additive_values + intercept

    # Clamp salt values to [0, 30]
    SALT_MIN_BOUND, SALT_MAX_BOUND = 0.0, 30.0
    salt_values = np.clip(salt_values, SALT_MIN_BOUND, SALT_MAX_BOUND)

    # Round to one decimal
    additive_values_rounded = np.round(additive_values, 1)
    salt_values_rounded = np.round(salt_values, 1)

    # Create the dataframe
    df_experiment = pd.DataFrame({
        'additive_wt_pct': additive_values_rounded,
        'salt_wt_pct': salt_values_rounded
    })
    
    # Get material from target recommendation
    material = df_target.loc[df_target['Parameter'] == 'Material', 'Suggested for Experiment'].values[0]
    
    # Find the min and max additive values
    min_add = df_experiment['additive_wt_pct'].min()
    max_add = df_experiment['additive_wt_pct'].max()
    
    # Find the salt values corresponding to min and max additive
    min_salt = df_experiment.loc[df_experiment['additive_wt_pct'] == min_add, 'salt_wt_pct'].values[0]
    max_salt = df_experiment.loc[df_experiment['additive_wt_pct'] == max_add, 'salt_wt_pct'].values[0]
    
    # Generate syringe_info as a dictionary with min and max formulations
    base_polymer = "9_PEGMEA_1_PEGDA"
    
    syringe_info = {
        'syringe_1': f"{base_polymer}_{min_add}_Aerosil380_{min_salt}_{material}",
        'syringe_2': f"{base_polymer}_{max_add}_Aerosil380_{max_salt}_{material}"
    }
    
    # Use string format for formation column
    df_experiment['formation'] = df_experiment.apply(
        lambda row: f"{base_polymer}_{row['additive_wt_pct']}_Aerosil380_{row['salt_wt_pct']}_{material}", 
        axis=1
    )
    
    return df_experiment, syringe_info

# Example usage:
# df_experiment, syringe_info = generate_experiment_points(df_target, df_acq)
# print("Experiment Points:")
# print(df_experiment)
# print("\nSyringe Formations:")
# print(f"Syringe 1: {syringe_info['syringe_1']}")
# print(f"Syringe 2: {syringe_info['syringe_2']}")

def main(csv_path, config_path=None):
    global scaler, model, acquisition, MATERIALS, PRINT_TYPE, BOUNDS, N_RANDOM_STARTS, N_ESTIMATORS, ACQUISITION
    
    # Load configuration if provided
    if config_path:
        configs = load_configs(config_path)
        if configs:
            # Update global variables from config
            if 'data' in configs:
                if 'materials' in configs['data']:
                    MATERIALS = configs['data']['materials']
                if 'print_type' in configs['data']:
                    PRINT_TYPE = configs['data']['print_type']
            
            if 'model' in configs and 'params' in configs['model']:
                if 'n_estimators' in configs['model']['params']:
                    N_ESTIMATORS = configs['model']['params']['n_estimators']
            
            if 'train' in configs:
                if 'acquisition' in configs['train']:
                    ACQUISITION = configs['train']['acquisition']
                if 'bounds' in configs['train']:
                    BOUNDS = configs['train']['bounds']
                if 'n_random_starts' in configs['train']:
                    N_RANDOM_STARTS = configs['train']['n_random_starts']
    
    print(f"Using configuration: MATERIALS={MATERIALS}, PRINT_TYPE={PRINT_TYPE}, N_ESTIMATORS={N_ESTIMATORS}, ACQUISITION={ACQUISITION}")
    
    # Load and preprocess data
    X, y, features, df = load_data(csv_path)
    
    # Initialize scaler and model
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X)
    
    model = build_model(input_dim=X.shape[1])
    model.fit(X_scaled, y)
    
    # Set acquisition function based on configuration
    if ACQUISITION == 'PI':
        acquisition = probability_improvement
    elif ACQUISITION == 'EI':
        acquisition = expected_improvement
    else:
        raise ValueError(f"Unknown acquisition function: {ACQUISITION}")
    
    # Recommend by target prediction
    print("Recommendation by Target Prediction (Ionic Conductivity):")
    df_target, best_target = recommend_by_target_prediction(X, y, scaler, model, acquisition)
    print(df_target)
    
    # Plot target prediction heatmap
    plot_target_prediction_heatmap(X, y, scaler, model, best_target['x_cont'], best_target['material'])
    
    # Recommend by acquisition function
    print("\nRecommendation by Acquisition Function:")
    df_acq, best_acq = recommend_by_acquisition(X, y, scaler, model, acquisition)
    print(df_acq)
    
    # Plot acquisition function heatmap
    plot_acquisition_heatmap(X, y, scaler, model, acquisition, best_acq['x_cont'], best_acq['material'])
    
    # Generate experiment points along the line between recommendations
    df_experiment, syringe_info = generate_experiment_points(df_target, df_acq)
    
    # Print experiment points
    print("\nGenerated Experiment Points:")
    print(df_experiment[['additive_wt_pct', 'salt_wt_pct', 'formation']])
    
    # Print syringe information
    print("\nSyringe Formations:")
    print(f"Syringe 1: {syringe_info['syringe_1']}")
    print(f"Syringe 2: {syringe_info['syringe_2']}")
    
    # Plot recommendations comparison with experiment points
    plot_recommendations_comparison(X, y, scaler, model, df_target, df_acq, df_experiment)

def plot_recommendations_comparison(X, y, scaler, model, df_target, df_acq, df_experiment=None):
    """
    Restored original style for comparison heatmap, no suggested points.
    Scientific notation on colorbar. Increased text sizes.
    """
    import numpy as np
    import matplotlib.pyplot as plt
    from matplotlib.colors import LogNorm

    # Extract recommendations
    target_material = df_target.loc[df_target['Parameter'] == 'Material', 'Suggested for Experiment'].values[0]
    mat_flags = np.array([1, 0]) if target_material == 'LiTFSI' else np.array([0, 1])

    # Grid generation for heatmap
    add_vals = np.linspace(0, 15, 500)
    salt_vals = np.linspace(0, 30, 500)
    grid = np.array([[a, s] for a in add_vals for s in salt_vals])

    print_type_flag = 1
    x_grid = np.hstack([
        grid,
        np.tile(mat_flags, (grid.shape[0], 1)),
        np.ones((grid.shape[0], 1)) * print_type_flag
    ])

    x_grid = scaler.transform(x_grid)
    mean, std = get_mean_std(model, x_grid)

    # Original style heatmap data
    Z = (-mean).reshape(len(add_vals), len(salt_vals))
    Z_display = -Z
    min_val = max(1e-6, Z_display.min())

    if min_val <= 0 or Z_display.max() <= 0:
        norm = None
    else:
        norm = LogNorm(vmin=min_val, vmax=Z_display.max())

    fig, ax = plt.subplots(figsize=(10, 8))
    im = ax.imshow(
        Z_display,
        origin='lower',
        extent=[salt_vals.min(), salt_vals.max(), add_vals.min(), add_vals.max()],
        aspect='auto',
        norm=norm
    )

    # Optional experiment points as in original
    if df_experiment is not None and len(df_experiment) > 0:
        ax.scatter(df_experiment['salt_wt_pct'], df_experiment['additive_wt_pct'],
                   color='blue', marker='*', s=150, edgecolor='black')
        df_sorted = df_experiment.sort_values('salt_wt_pct')
        ax.plot(df_sorted['salt_wt_pct'], df_sorted['additive_wt_pct'], 'b--', alpha=0.7)

    # No suggested points, no legend
    ax.set_xlabel('Salt Weight (%)', fontsize=24)
    ax.set_ylabel('Additive Weight (%)', fontsize=24)
    ax.set_title(f'Ionic Conductivity Heatmap with Recommendations for {target_material}', fontsize=20)
    ax.tick_params(axis='both', which='major', labelsize=24)

    cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    if norm is not None:
        import numpy as np
        log_min = np.log10(min_val)
        log_max = np.log10(Z_display.max())
        tick_locations = np.logspace(log_min, log_max, 5)
        cbar.set_ticks(tick_locations)
        # Scientific notation labels
        cbar.set_ticklabels([f"{x:.1e}" for x in tick_locations])
        cbar.set_label('Ionic Conductivity', fontsize=14)
    else:
        cbar.set_label('Ionic Conductivity', fontsize=14)
    cbar.ax.tick_params(labelsize=12)

    plt.tight_layout()
    plt.show()
    #%%

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Polymer optimization with Bayesian optimization')
    parser.add_argument('--csv_path', type=str, required=True, help='Path to the CSV file with polymer data')
    parser.add_argument('--config_path', type=str, help='Path to configuration YAML file')
    args = parser.parse_args()
    
    main(args.csv_path, args.config_path)