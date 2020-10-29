from sklearn.preprocessing import StandardScaler
from flask import Flask, request, jsonify
from flask_cors import CORS
import joblib


app = Flask(__name__)
cors = CORS(app, resources={r"/*": {"origins": "*"}})
loaded_model = joblib.load('finalized_model_2018.sav')
sc = joblib.load('scaler.bin')


@app.route('/')
def hello():
    return 'Hello World!'


# /predict?cn=15&ch=25&lc=26&mt=12
@app.route('/predict')
def predict():
    cn = request.args['cn']
    ch = request.args['ch']
    lc = request.args['lc']
    mt = request.args['mt']
    x_test = [[cn, ch, lc, mt]]
    x_test = sc.transform(x_test)
    prediction = loaded_model.predict(x_test)
    return jsonify({
        "cn": prediction[0][0],
        "ch": prediction[0][1],
        "lc": prediction[0][2],
        "mt": prediction[0][3]
    })


if __name__ == '__main__':
    app.run(host='127.0.0.1', port=8080)
