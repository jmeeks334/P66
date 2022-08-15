
import smtplib
from flask import Flask, render_template, request, jsonify, redirect
import csv
import sqlite3
import requests
import json
from datetime import datetime
import statistics

# Configure app
app = Flask(__name__)


@app.route("/")
def index():
    conn = sqlite3.connect('//phillips66.net/PO_SHARED/ENVIRON/AIR - EF17/NTDahs/wwwroot/main/ARG.db')
    c = conn.cursor()
    c.row_factory = sqlite3.Row
    c.execute("SELECT *, MAX(inst) FROM cyl GROUP BY cems;")
    cyltbl = c.fetchall()
    c.execute("SELECT * FROM cyl ORDER BY id DESC;")
    allcyl = c.fetchall()
    c.execute("SELECT * FROM log ORDER BY id DESC LIMIT 100;")
    hunlog = c.fetchall()
    c.execute("SELECT * FROM log ORDER BY id DESC")
    alllog = c.fetchall()
    
    return render_template("index.html", cyltbl=cyltbl, allcyl=allcyl, hunlog=hunlog, alllog=alllog)

@app.route("/CGA")
def CGA():
    return render_template("CGA.html")

@app.route("/cyl", methods=["POST"])
def new():
    conn = sqlite3.connect('//phillips66.net/PO_SHARED/ENVIRON/AIR - EF17/NTDahs/wwwroot/main/ARG.db')
    c = conn.cursor()
    conc = request.form.get("conc")
    CEMS = request.form.get("CEMSCyl")
    tech = request.form.get("techcyl")
    ID = request.form.get("cyl")
    exp = request.form.get("exp")
    inst = request.form.get("inst")
    tech = request.form.get("techcyl")
    H2 = request.form.get("H2Conc")
    Methane = request.form.get("MethaneConc")
    Ethane = request.form.get("EthaneConc")
    Ethylene = request.form.get("EthyleneConc")
    Propane = request.form.get("PropaneConc")
    Propylene = request.form.get("PropyleneConc")
    Butane = request.form.get("ButaneConc")
    Isobutane = request.form.get("IsobutaneConc")
    Isobutylene = request.form.get("IsobutyleneConc")
    Pentane = request.form.get("PentaneConc")
    N2 = request.form.get("N2Conc")
    CO = request.form.get("COConc")
    CO2 = request.form.get("CO2Conc")
    ip = request.remote_addr
    
    if not CEMS or not ID or not exp or not inst or not tech:
        return render_template("failure.html")
    #if "BTU" in CEMS and not H2 or not Methane:
        #return CEMS
    if H2 and Methane and Ethane and Ethylene and Propane and Propylene and Butane and Isobutane and Isobutylene and Pentane and N2 and CO and CO2:
        NHV = ((float(Methane)*896/100) + (float(Ethane)*1595/100) + (float(Ethylene)*1477/100) + (float(Butane)*2968/100) + (float(Isobutylene)*2758/100) + 
        (float(Isobutane)*2957/100) + (float(Propane)*2281/100) + (float(Propylene)*2150/100) + (float(Pentane)*3655/100) + (float(H2)*1212/100) + 
        (float(CO)*316/100))
        c.execute("INSERT INTO btu (cems, cylid, exp, inst, tech, H2, Methane, Ethane, Ethylene, Propane, Propylene, Butane, Isobutane, Isobutylene, Pentane, N2, CO, CO2) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);", (CEMS, ID, exp, inst, tech, H2, Methane, Ethane, Ethylene, Propane, Propylene, Butane, Isobutane, Isobutylene, Pentane, N2, CO, CO2))
        c.execute("INSERT INTO cyl (cems, conc, cylid, exp, inst, tech) VALUES (?,?,?,?,?,?);", (CEMS, NHV, ID, exp, inst, tech))
        conn.commit()
        conn.close()
    if conc:
        c.execute("INSERT INTO cyl (cems, conc, cylid, exp, inst, tech) VALUES (?,?,?,?,?,?);", (CEMS, conc, ID, exp, inst, tech))
        conn.commit()
        conn.close()

    with open ("cylinders.csv", "a", newline='') as file:
        fieldnames = ['CEMS','conc', 'id', 'exp', 'inst', 'tech']
        writer = csv.DictWriter(file, fieldnames=fieldnames)
        writer.writerow({'CEMS': CEMS, 
                        'conc': conc, 
                        'id': ID, 
                        'exp': exp, 
                        'inst': inst, 
                        'tech': tech})
    
    #message = ( tech + " just input a new cylinder\n" + 
                #CEMS + " " + 
                #conc + " " + 
                #ID + " - Installed on: " + inst + "\n" + ip )
    #server = smtplib.SMTP("148.89.166.11")
    #server.starttls()
    #server.sendmail("PCRpython@p66.com", "jeremy.c.meeks@p66.com", message)
    return render_template("success.html", CEMS=CEMS)

@app.route("/maint", methods=["POST"])
def log():
    conn = sqlite3.connect('//phillips66.net/PO_SHARED/ENVIRON/AIR - EF17/NTDahs/wwwroot/main/ARG.db')
    c = conn.cursor()
    log = request.form.get("maintlog")
    log = log.strip()
    CEMS = request.form.get("CEMSMaint")
    tech = request.form.get("techmaint")
    maintdt = request.form.get("maintdt")
    maintstrt = request.form.get("maintstrt")
    maintend = request.form.get("maintend")
    ip = request.remote_addr
    if not log or not CEMS or not tech or not maintdt or not maintstrt or not maintend:
        return render_template("failure.html")
    #message = tech + " just entered a log\n" + CEMS + " " + log + "\n" + ip
    #server = smtplib.SMTP("148.89.166.11")
    #server.starttls()
    #server.sendmail("PCRpython@p66.com", "jeremy.c.meeks@p66.com", message)
    with open ("log.csv", "a", newline='') as file:
        fieldnames = ['Analyzer', 'Date', 'Start', 'End', 'Initials', 'Log']
        writer = csv.DictWriter(file, fieldnames=fieldnames)
        writer.writerow({'Analyzer': CEMS,
                        'Date': maintdt,
                        'Start': maintstrt,
                        'End': maintend,
                        'Initials': tech,
                        'Log': log})
    c.execute("INSERT INTO log (anal, dt, strt, end, tech, log) VALUES (?,?,?,?,?,?);", (CEMS, maintdt, maintstrt, maintend, tech, log))
    conn.commit()
    conn.close()
    return render_template("success.html")

@app.route("/cyledit", methods=["POST"])
def cyledit():
    conn = sqlite3.connect('//phillips66.net/PO_SHARED/ENVIRON/AIR - EF17/NTDahs/wwwroot/main/ARG.db')
    c = conn.cursor()
    id = request.form.get("idcyledit")
    conc = request.form.get("concedit")
    CEMS = request.form.get("CEMSCyledit")
    cylid = request.form.get("cylidedit")
    exp = request.form.get("expedit")
    inst = request.form.get("instedit")
    tech = request.form.get("techcyledit")
    c.execute("UPDATE cyl SET conc=(?), cylid=(?), exp=(?), inst=(?), tech=(?) WHERE id=(?);", (conc, cylid, exp, inst, tech, id))
    conn.commit()
    conn.close()
    return render_template("success.html")

@app.route("/logedit", methods=["POST"])
def logedit():
    conn = sqlite3.connect('//phillips66.net/PO_SHARED/ENVIRON/AIR - EF17/NTDahs/wwwroot/main/ARG.db')
    c = conn.cursor()
    id = request.form.get("idlogedit")
    log = request.form.get("maintlogedit")
    log = log.strip()
    anal = request.form.get("CEMSMaintedit")
    tech = request.form.get("techmaintedit")
    maintdt = request.form.get("maintdtedit")
    maintstrt = request.form.get("maintstrtedit")
    maintend = request.form.get("maintendedit")
    c.execute("UPDATE log SET log=(?), dt=(?), strt=(?), end=(?), tech=(?) WHERE id=(?);", (log, maintdt, maintstrt, maintend, tech, id))
    conn.commit()
    conn.close()
    return render_template("success.html")
    

@app.route("/cyldel", methods=["POST"])
def cyldel():
    conn = sqlite3.connect('//phillips66.net/PO_SHARED/ENVIRON/AIR - EF17/NTDahs/wwwroot/main/ARG.db')
    c = conn.cursor()
    cyl = request.form.get("delcyl")
    ip = request.remote_addr
    c.execute("SELECT * FROM cyl WHERE id=(?);", (cyl,))
    rem = c.fetchall()
    listToStr = ' '.join([str(elem) for elem in rem])
    message = "CYLINDER REMOVED BY: " + ip + " \n" + listToStr
    server = smtplib.SMTP("148.89.166.11")
    server.starttls()
    server.sendmail("PCRpython@p66.com", "jeremy.c.meeks@p66.com", message)
    c.execute("DELETE FROM cyl WHERE id=(?);", (cyl,))
    conn.commit()
    conn.close()
    
    return render_template("success.html")
    

@app.route("/logdel", methods=["POST"])
def logdel():
    conn = sqlite3.connect('//phillips66.net/PO_SHARED/ENVIRON/AIR - EF17/NTDahs/wwwroot/main/ARG.db')
    c = conn.cursor()
    log = request.form.get("dellog")
    ip = request.remote_addr
    c.execute("SELECT * FROM log WHERE id=(?);", (log,))
    rem = c.fetchall()
    listToStr = ' '.join([str(elem) for elem in rem])
    message = "LOG REMOVED BY: " + ip + "\n" + listToStr
    server = smtplib.SMTP("148.89.166.11")
    server.starttls()
    server.sendmail("PCRpython@p66.com", "jeremy.c.meeks@p66.com", message)
    c.execute("DELETE FROM log WHERE id=(?);", (log,))
    conn.commit()
    conn.close()
    return render_template("success.html")

@app.route("/suggestions", methods=["GET"])
def suggestions():
    return render_template("index2.html")

@app.route("/subsug", methods=["POST"])
def subsug():
    ip = request.remote_addr
    sug = request.form.get("websug")
    message = ip + " has a suggestion:" " \n" + sug
    server = smtplib.SMTP("148.89.166.11")
    server.starttls()
    server.sendmail("PCRdashboard@p66.com", "jeremy.c.meeks@p66.com", message)
    return render_template("success2.html")

@app.route("/tnk")
def tnkX():
    conn = sqlite3.connect('//phillips66.net/PO_SHARED/ENVIRON/AIR - EF17/NTDahs/wwwroot/main/Tank.db')
    c = conn.cursor()
    c.row_factory = sqlite3.Row
    c.execute("SELECT * FROM clean;")
    tnkCln = c.fetchall()
    c.execute("SELECT * FROM tanks;")
    tnks = c.fetchall()
    c.execute("SELECT * FROM stocks;")
    stks = c.fetchall()
    return render_template("TankX.html", tnkCln=tnkCln, tnks=tnks, stks=stks)

@app.route("/tnksubmit", methods=["POST"])
def tnksub():
    conn = sqlite3.connect('//phillips66.net/PO_SHARED/ENVIRON/AIR - EF17/NTDahs/wwwroot/main/Tank.db')
    c = conn.cursor()
    frm = request.form
    frmList = list(frm.keys())
    Stg2LEL = []
    Stg2FanRun = [] 
    Stg2FanRate = []
    Stg3LEL = []
    Stg3FanRun = []
    Stg3FanRate = []
    Stg4LEL = []
    Stg4FanRun = []
    Stg4FanRate = []
    
    for i in frm:
        for j in range(1,4):
                for k in range (0,4):
                    LEL2 = "S2" + str(j) + "LELRd" + str(k)
                    LEL3 = "S3" + str(j) + "LELRd" + str(k)
                    LEL4 = "S4" + str(j) + "LELRd" + str(k)
                    FanRate2 = "S2FanRate" + str(k)
                    FanRate3 = "S3FanRate" + str(k)
                    FanRate4 = "S4FanRate" + str(k)
                    FanRun2 = "S2FanRun" + str(k)
                    FanRun3 = "S3FanRun" + str(k)
                    FanRun4 = "S4FanRun" + str(k)

                    if i == LEL2 and frm.get(i):
                        Stg2LEL.append(float(frm.get(i)))
                    elif i == LEL3 and frm.get(i):
                        Stg3LEL.append(float(frm.get(i)))
                    elif i == LEL4 and frm.get(i):
                        Stg4LEL.append(float(frm.get(i)))
                    if i == FanRate2 and frm.get(i):
                        Stg2FanRate.append(float(frm.get(i)))
                    elif i == FanRate3 and frm.get(i):
                        Stg3FanRate.append(float(frm.get(i)))
                    elif i == FanRate4 and frm.get(i):
                        Stg4FanRate.append(float(frm.get(i)))
                    if i == FanRun2 and frm.get(i):
                        Stg2FanRun.append(float(frm.get(i)))
                    elif i == FanRun3 and frm.get(i):
                        Stg3FanRun.append(float(frm.get(i)))
                    elif i == FanRun4 and frm.get(i):
                        Stg4FanRun.append(float(frm.get(i)))
    
    frm = frm.to_dict()

    if Stg2LEL:
        Stg2LELMean = statistics.mean(Stg2LEL)
    else:
        Stg2LELMean = 'Blank'

    if Stg3LEL:
        Stg3LELMean = statistics.mean(Stg3LEL)
    else:
        Stg3LELMean = 'Blank'

    if Stg4LEL:      
        Stg4LELMean = statistics.mean(Stg4LEL)
    else:
        Stg4LELMean = 'Blank'

    if Stg2FanRate:
        Stg2FanRateMean = statistics.mean(Stg2FanRate)
    else:
        Stg2FanRateMean = 'Blank'

    if Stg3FanRate:
        Stg3FanRateMean = statistics.mean(Stg3FanRate)
    else:
        Stg3FanRateMean = 'Blank'

    if Stg4FanRate:      
        Stg4FanRateMean = statistics.mean(Stg4FanRate)
    else:
        Stg4FanRateMean = 'Blank'

    if Stg2FanRun:
        Stg2FanRunMean = statistics.mean(Stg2FanRun)
    else:
        Stg2FanRunMean = 'Blank'

    if Stg3FanRun:
        Stg3FanRunMean = statistics.mean(Stg3FanRun)
    else:
        Stg3FanRunMean = 'Blank'

    if Stg4FanRun:      
        Stg4FanRunMean = statistics.mean(Stg4FanRun)
    else:
        Stg4FanRunMean = 'Blank'
    
    frm.update({'Stg2LELMean' : Stg2LELMean, 'Stg3LELMean' : Stg3LELMean, 'Stg4LELMean' : Stg4LELMean, 
                'Stg2FanRateMean' : Stg2FanRateMean, 'Stg3FanRateMean' : Stg3FanRateMean, 'Stg4FanRateMean' : Stg4FanRateMean,
                'Stg2FanRunMean' : Stg2FanRunMean, 'Stg3FanRunMean' : Stg3FanRunMean, 'Stg4FanRunMean' : Stg4FanRunMean})
    
    frmList = list(frm.keys())
    frmVal = list(frm.values())
    table = "clean"
    plc = ':'+', :'.join(frm.keys())
    columns = ', '.join(frm.keys())
    idTest = columns[0:2]
    recID = frmVal[0]
    server = smtplib.SMTP("148.89.166.11")
    server.starttls()
    ip = request.remote_addr
    Tank = request.form.get("TankNo")
    
    if (idTest == "id"):
        sql = "Update clean set ( %s ) = ( %s ) where id=( %s )" % (columns, plc, recID)
        message = "An existing tank cleaning form was updated for:" + " \n" + Tank + "\n\n" + "By: " + ip
    else:
        sql = "INSERT INTO clean ( %s ) VALUES ( %s )" % (columns, plc)
        message = "A new tank cleaning form was entered for:" + " \n" + Tank + "\n\n" + "By: " + ip
    
   
    server.sendmail("PCRTankCleaning@p66.com", "jeremy.c.meeks@p66.com; matt.evans@p66.com", message)
    c.execute(sql, list(frm.values())) 
    conn.commit()
    conn.close()
    return redirect("http://pcr.phillips66.net/ops")


@app.route("/roster")
def roster():
    return render_template("roster.html")

@app.route("/rostersub", methods=["POST"])
def rstrsub():
    form = json.dumps(request.form)
    print(form)
    d = { "date" : datetime.now().strftime("%Y-%m-%d %H:%M")}
    form = json.loads(form)
    form.update(d)
    form = json.dumps(form)
    print(form)
    requests.post(headers={"Content-Type": "application/json"}, url = "https://prod-01.westus.logic.azure.com:443/workflows/2ddf642e5cf24a36830ee3a0e3054b87/triggers/manual/paths/invoke?api-version=2016-06-01&sp=%2Ftriggers%2Fmanual%2Frun&sv=1.0&sig=c5-cT6eTagaqVWGQTvMcAjRLOtnyLN9xp8F5BwlhqqU", data = form)
    return render_template("success3.html")

@app.route("/mtg")
def mtg():
    return render_template('mtg.html')

@app.route("/mtgQRGen", methods=["POST"])
def mtgQRGen():
    mtgName = request.form.get("mtgName")
    mtgDt = request.form.get("mtgDt")
    now = str(round(datetime.now().timestamp()))
    print(now)
    code = { "code" : now}
    urlName = "https://chart.googleapis.com/chart?cht=qr&chs=400x400&chl=googlechrome://awsponwbip4591/roster?mtgName=" + mtgName + "%26mtgDt=" + mtgDt + "%26mtgCode=" + now
    fileName = "/" + mtgDt + mtgName + ".png"
    url = {"url" : urlName}
    fil = {"fil" : fileName}
    form = json.dumps(request.form)
    form = json.loads(form)
    form.update(url)
    form.update(fil)
    form.update(code)
    form = json.dumps(form)
    requests.post(headers={"Content-Type": "application/json"}, url = "https://prod-73.westus.logic.azure.com:443/workflows/9527f4814243485aa442685fe0c3c91a/triggers/manual/paths/invoke?api-version=2016-06-01&sp=%2Ftriggers%2Fmanual%2Frun&sv=1.0&sig=23olrv4tlpP0v5IUoO3umv1owvauWqpsoSU-Nm5SnHo", data = form)

    return render_template("QR.html", urlName=urlName)



if __name__ == "__main__":
    app.run(threaded=True)