#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <Eigen/Dense>
#include <fstream>
#include <sstream>
#include <cmath>

using namespace Eigen;
using namespace std;

// Function to read a CSV file into an Eigen Matrix
MatrixXd read_csv(const string& filename) {
    ifstream file(filename);
    vector<vector<double>> data;
    string line, value;

    while (getline(file, line)) {
        stringstream ss(line);
        vector<double> row;
        while (getline(ss, value, ',')) {
            row.push_back(stod(value));
        }
        data.push_back(row);
    }

    int rows = data.size();
    int cols = data[0].size();
    MatrixXd matrix(rows, cols);
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            matrix(i, j) = data[i][j];
        }
    }
    return matrix;
}

// Function to calculate cosine similarity
VectorXd calculate_cos_sim(const MatrixXd& aligned_data) {
    int rows = aligned_data.rows();
    VectorXd cos_sim(rows);

    for (int i = 0; i < rows; i++) {
        VectorXd vec1 = aligned_data.row(i);
        double norm_vec1 = vec1.norm();

        VectorXd vec2 = aligned_data.row(i);  // Placeholder for second vector
        double norm_vec2 = vec2.norm();

        cos_sim(i) = vec1.dot(vec2) / (norm_vec1 * norm_vec2);
    }
    return cos_sim;
}

// Function to process DIAnn report
MatrixXd process_diann_report(const string& diann_report, double base) {
    MatrixXd report_data = read_csv(diann_report);
    return report_data.array().log1p();  // Log transformation (log1p for numerical stability)
}

// Function to process the library
MatrixXd process_lib(const string& lib, const string& lib_type, double base) {
    MatrixXd lib_data = read_csv(lib);
    return lib_data.array().log1p();
}

// Function to align DIAnn report with the library
MatrixXd align_diann_report_lib_frgs(const MatrixXd& diann_report, const MatrixXd& lib, double tol) {
    return (diann_report - lib).cwiseAbs().array() < tol;  // Example placeholder for alignment logic
}

// Main function to calculate cosine similarity
VectorXd calculate_cosine_sim_4_DIAnn(const string& diann_report, const string& lib, 
                                     const string& lib_type = "DDA", double tol = 0.05, double base = 0) {
    MatrixXd diann_report_proc = process_diann_report(diann_report, base);
    MatrixXd lib_proc = process_lib(lib, lib_type, base);
    MatrixXd aligned_data = align_diann_report_lib_frgs(diann_report_proc, lib_proc, tol);
    return calculate_cos_sim(aligned_data);
}

int main() {
    string diann_report = "diann_report.csv";
    string lib = "library.csv";
    
    VectorXd cosine_similarity = calculate_cosine_sim_4_DIAnn(diann_report, lib);
    
    cout << "Cosine Similarity Computed!" << endl;
    
    return 0;
}
