#%%
import sys
import os
import pandas as pd
import multiprocessing as mp
sys.path.append(r'C:\Users\jimenez45\Documents\Git\23-spoc_code')
from EIS_process_compiled_jimenez45 import *
    
def autoprocess_EIS():
    folder_path = r'C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_new_files_converted'

    files = os.listdir(folder_path)
    files_series = pd.Series(files)
    files_to_process = pd.DataFrame({
        'files': files_series.str.replace('.csv', ''),
        'filepath': [os.path.join(folder_path, f) for f in files]
    })

    files_done = pd.DataFrame({'files': os.listdir('C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\eis_results')})
    
    # Fix: Ensure 'files' column is string type before using .str accessor
    files_done['files'] = files_done['files'].astype(str)
    
    files_done = files_done[files_done['files'].str.contains('.csv')]
    files_done['files'] = files_done['files'].str.replace('__randles.csv', '')

    # Remove duplicates if needed
    files_done = files_done.drop_duplicates(subset=['files'])

    # Now merge and create the not_processed flag
    df = pd.merge(files_to_process, files_done, on='files', how='left', indicator=True)
    df['not_processed'] = df['_merge'] == 'left_only'
    df = df.drop('_merge', axis=1)

    df_filtered = df[df['not_processed'] == True].reset_index(drop=True)

    output_folder = r'C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\eis_results'

    # Track successful and failed files
    successful_files = []
    failed_files = []
    
    for x in range(len(df_filtered)):
        path = df_filtered['filepath'][x]
        
        try:
            headcount = EIS_analysis.get_headcount(path)
            R0, R1, C1, Wo1_0, Wo1_1 = EIS_analysis.get_guesses(path, headcount)
            EIS_analysis.apply_randles_fits_wrapper(
                    path, output_folder,
                    R0, R1, 
                    Wo1_0, 
                    Wo1_1, C1, headcount, 
                    True, False)
            
            successful_files.append(path)
            
        except Exception as e:
            failed_files.append({'filepath': path, 'error': str(e)})
            continue
    
    # Save failed files list if any
    if failed_files:
        failed_df = pd.DataFrame(failed_files)
        failed_df.to_csv('failed_files_log.csv', index=False)
    
    return successful_files, failed_files

    # num_processes = 4
    # pool = mp.Pool(processes=num_processes)

    # # batch process all of the files in the folder
    # with mp.Pool(processes=num_processes) as pool:
    #     args = [(filepath, output_folder) for filepath in df['filepath']]
    #     results = pool.starmap(EIS_analysis_wrapper, args)

def EIS_analysis_wrapper(file, output_folder):
    
    headcount = EIS_analysis.get_headcount(file) # Either use this or specify
    R0, R1, C1, Wo1_0, Wo1_1 = EIS_analysis.get_guesses(file, headcount)
    EIS_analysis.apply_randles_fits_wrapper(
        file, output_folder,
        R0, R1, 
        Wo1_0, 
        Wo1_1, C1, headcount, 
        True, False
    )
#%%

autoprocess_EIS()

#%%