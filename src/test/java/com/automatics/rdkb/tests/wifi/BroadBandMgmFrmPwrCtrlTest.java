/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.wifi;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.enums.BroadBandManagementPowerControlEnum;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandMgmFrmPwrCtrlTest extends AutomaticsTestBase{
	
	/**
    *
    * Test Case # 3: Verify the reduction in power level of 2.4GHz WiFi Access Point does not impact the non-management
    * frames.
    *
    * <p>
    * STEPS:
    * </p>
    * <ol>
    * <li>S1) Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with value 0
    * </li>
    * <li>S2) Verify connecting the WiFi Client to 2.4GHz Radio.</li>
    * <li>S3) Verify the internet connection in the Connected Client</li>
    * <li>S4) Verify the quality of the internet connection.</li>
    * <li>S5) Verify disconnecting the WiFi Client</li>
    * <li>S6) Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with value -15
    * </li>
    * <li>S7) Verify connecting the WiFi Client to 2.4GHz Radio.</li>
    * <li>S8) Verify the internet connection in the Connected Client</li>
    * <li>S9) Verify the quality of the internet connection does not deteriorate on changing the management frame power
    * level.</li>
    * <li>S10) Verify disconnecting the WiFi Client</li>
    * </ol>
    *
    * @author BALAJI V
    * @refactor Govardhan
    * 
    * @param device
    *            {@link Dut}
    */
   @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
   @TestDetails(testUID = "TC-RDKB-WIFI-5053")
   public void testMgmtFramePwrControlFor2GHzRadio(Dut device) {
	String testCaseId = "TC-RDKB-WIFI-553";
	int stepNumber = 1;
	boolean result = false;
	String step = null;
	String errorMessage = null;
	String webPaParameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_1
		.getWebPaParamMgmtPower();
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-5053");
	    LOGGER.info("TEST DESCRIPTION: Management Frame Power control for 2.4GHz");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info(
		    "STEP 1: Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with value 0");
	    LOGGER.info("STEP 2: Verify connecting the WiFi Client to 2.4GHz Radio.");
	    LOGGER.info("STEP 3: Verify the internet connection in the Connected Client.");
	    LOGGER.info("STEP 4: Verify the quality of the internet connection.");
	    LOGGER.info("STEP 5: Verify disconnecting the WiFi Client from 2.4GHz Radio.");
	    LOGGER.info(
		    "STEP 6: Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with value -15.");
	    LOGGER.info("STEP 7: Verify connecting the WiFi Client to 2.4GHz Radio.");
	    LOGGER.info("STEP 8: Verify the internet connection in the Connected Client.");
	    LOGGER.info("STEP 9: Verify the quality of the internet connection.");
	    LOGGER.info("STEP 10: Verify disconnecting the WiFi Client from 2.4GHz Radio.");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1 : Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with
	     * value 0.
	     */
	    step = "S" + stepNumber;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT VALUE TO '0'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT VALUE TO '0' USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_ManagementFramePowerControl'.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT VALUE MUST BE SET TO '0' SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT VALUE TO '0'";
	    String expectedValue = "0";
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webPaParameterName,
		    BroadBandTestConstants.CONSTANT_1, expectedValue);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result
			    ? "SUCCESSFULLY SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHZ PRIVATE WI-FI ACCESS POINT VALUE TO '0'"
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 2 : Verify connecting the WiFi Client to 2.4GHz Radio.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    // Connected client instance
	    Dut connectedClientSettop = null;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT TO 2.4GHZ RADIO.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WIFI CLIENT TO 2.4GHz RADIO.");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED :  WIFI CLIENT MUST BE CONNECTED TO 2.4GHz RADIO SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO CONNECT THE WIFI CLIENT TO 2.4GHz RADIO.";
	    connectedClientSettop = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    result = null != connectedClientSettop;
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "WIFI CLIENT CONNECTED TO 2.4GHz RADIO SUCCESSFULLY." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 3 : Verify the internet connection in the Connected Client
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :  VERIFY THE INTERNET CONNECTION IN THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : EXECUTE COMMAND WINDOWS : ping www.google.com -n 1 | grep 'Reply from' or LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' ON THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  CONNECTED CLIENT SHOULD BE ABLE TO ACCESS THE INTERNET SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO ACCESS THE INTERNET FROM THE CONNECTED CLIENT.";
	    result = ConnectedNattedClientsUtils.verifyNetworkConnection(connectedClientSettop, tapEnv, true);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "CONNECTED CLIENT IS ABLE TO ACCESS THE INTERNET SUCCESSFULLY." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 4 : Verify the quality of the internet connection.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    int avgResponseTimeInitial = 0;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :   VERIFY THE QUALITY OF THE INTERNET CONNECTION IN THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : RETRIEVE THE AVERAGE TIME TAKEN TO ACCESS 'www.google.com' USING PING COMMAND.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED :  THE AVERAGE RESPONSE TIME MUST BE AVAILABLE.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO RETRIEVE THE AVERAGE TIME TAKEN TO PING 'www.google.com'";
	    avgResponseTimeInitial = ConnectedNattedClientsUtils.getPingResponseAvgTime(connectedClientSettop, tapEnv,
		    BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS);
	    result = (avgResponseTimeInitial > 0);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result
			    ? "SUCCESSFULLY RETRIEVED THE AVERAGE TIME TAKEN TO ACCESS 'www.google.com'. THE AVERAGE RESPONSE TIME : "
				    + avgResponseTimeInitial + " milli seconds."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 5 : Verify disconnecting the WiFi Client from 2.4GHz Radio.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");

	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :  VERIFY DISCONNECTING THE WIFI CLIENT FROM 2.4GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : DISCONNECT THE WIFI CLIENT FROM 2.4GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  WIFI CLIENT MUST BE DISCONNECTED FROM 2.4GHZ RADIO SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    // String to store the ssid
	    String ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    errorMessage = "UNABLE TO RETRIEVE 2.4GHz WIFI NETWORK SSID FROM THE ROUTER DEVICE.";
	    if (CommonMethods.isNotNull(ssid)) {
		errorMessage = "UNABLE TO DISCONNECT WIFI CLIENT FROM 2.4GHz RADIO CONNECTION.";
		result = ConnectedNattedClientsUtils.disconnectSSID(connectedClientSettop, tapEnv, ssid);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "SUCCESSFULLY DISCONNECTED THE WIFI CLIENT FROM 2.4GHz RADIO CONNECTION." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * STEP 6 : Verify setting the management frame power level for 2.4 GHz Private WiFi Access Point (1) with
	     * value -15
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT VALUE TO '-15'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT VALUE TO '-15' USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_ManagementFramePowerControl'.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT VALUE MUST BE SET TO '-15' SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    expectedValue = "-15";
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webPaParameterName,
		    BroadBandTestConstants.CONSTANT_1, expectedValue);
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WI-FI ACCESS POINT VALUE TO '-15'";
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result
			    ? "SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHZ PRIVATE WI-FI ACCESS POINT VALUE TO '-15' SUCCESSFULLY."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 7 : Verify connecting the WiFi Client to 2.4GHz Radio.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    // Connected client instance
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT TO 2.4GHZ RADIO.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WIFI CLIENT TO 2.4GHz RADIO.");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED :  WIFI CLIENT MUST BE CONNECTED TO 2.4GHz RADIO SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO CONNECT THE WIFI CLIENT TO 2.4GHz RADIO.";
	    connectedClientSettop = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    result = null != connectedClientSettop;
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "WIFI CLIENT CONNECTED TO 2.4GHz RADIO SUCCESSFULLY." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 8 : Verify the internet connection in the Connected Client
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :  VERIFY THE INTERNET CONNECTION IN THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : EXECUTE COMMAND WINDOWS : ping www.google.com -n 1 | grep 'Reply from' or LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' ON THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  CONNECTED CLIENT SHOULD BE ABLE TO ACCESS THE INTERNET SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO ACCESS THE INTERNET FROM THE CONNECTED CLIENT.";
	    result = ConnectedNattedClientsUtils.verifyNetworkConnection(connectedClientSettop, tapEnv, true);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "CONNECTED CLIENT IS ABLE TO ACCESS THE INTERNET SUCCESSFULLY." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 9 : Verify the quality of the internet connection does not deteriorate on changing the management
	     * frame power level.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    int avgResponseTimeFinal = 0;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :   VERIFY THE QUALITY OF THE INTERNET CONNECTION IN THE CONNECTED CLIENT ON CHANGING THE MANAGEMENT FRAME POWER LEVEL.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : RETRIEVE THE AVERAGE TIME TAKEN TO ACCESS 'www.google.com' USING PING COMMAND.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  THE AVERAGE RESPONSE TIME MUST BE AVAILABLE AND IT MUST NOT CHANGE DRASTICALLY");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO RETRIEVE THE AVERAGE TIME TAKEN TO PING 'www.google.com'";
	    avgResponseTimeFinal = ConnectedNattedClientsUtils.getPingResponseAvgTime(connectedClientSettop, tapEnv,
		    BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS);
	    result = avgResponseTimeFinal > 0;
	    if (result) {
		errorMessage = "UNABLE TO VERIFY THE QUALITY OF THE INTERNET CONNECTION DOES NOT DEGRADE ON CHANGING THE MGMT FRAME POWER LEVEL.PING AVG RESPONSE TIME (DEFAULT) = "
			+ avgResponseTimeInitial + ", PING AVG RESPONSE TIME (AFTER CHANGING POWER LEVEL) = "
			+ avgResponseTimeFinal;
		result = (avgResponseTimeFinal
			- avgResponseTimeInitial) <= BroadBandTestConstants.PING_RESPONSE_AVG_TIME_THRESHOLD;
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result
			    ? "SUCCESSFULLY VERIFIED THE QUALITY OF INTERNET CONNECTION DOES NOT DEGRADE ON CHANGING THE MGMT FRAME POWER LEVEL. PING AVG RESPONSE TIME (DEFAULT) = "
				    + avgResponseTimeInitial
				    + ", PING AVG RESPONSE TIME (AFTER CHANGING POWER LEVEL) = " + avgResponseTimeFinal
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * STEP 10 : Verify disconnecting the WiFi Client from 2.4GHz Radio.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");

	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :  VERIFY DISCONNECTING THE WIFI CLIENT FROM 2.4GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : DISCONNECT THE WIFI CLIENT FROM 2.4GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  WIFI CLIENT MUST BE DISCONNECTED FROM 2.4GHz RADIO SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    // String to store the ssid
	    ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    errorMessage = "UNABLE TO RETRIEVE 2.4GHz WIFI NETWORK SSID FROM THE ROUTER DEVICE.";
	    if (CommonMethods.isNotNull(ssid)) {
		errorMessage = "UNABLE TO DISCONNECT WIFI CLIENT FROM 2.4GHz RADIO CONNECTION.";
		result = ConnectedNattedClientsUtils.disconnectSSID(connectedClientSettop, tapEnv, ssid);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "SUCCESSFULLY DISCONNECTED THE WIFI CLIENT FROM 2.4GHz RADIO CONNECTION." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE VALIDATING THE QUALITY OF THE LINK AFTER SETTING THE 2.4GHz MGMT FRAME POWER LEVEL: "
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info(
		    "POST-CONDITION : STEP 1 : SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT VALUE TO '0' USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_ManagementFramePowerControl'.");
	    errorMessage = "UNABLE TO RESET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WIFI ACCESS POINT VALUE TO '0'";
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webPaParameterName,
		    BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_ZERO);
	    LOGGER.info("SET THE MANAGEMENT FRAME POWER LEVEL FOR 2.4GHz PRIVATE WI-FI ACCESS POINT VALUE TO '0': "
		    + result);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    LOGGER.info("COMPLETED TEST CASE: TC-RDKB-WIFI-5053");
	    LOGGER.info("#######################################################################################");
	}
   }
   
   /**
   *
   * Test Case # 4: Verify the reduction in power level of 5GHz WiFi Access Point does not impact the non-management
   * frames.
   *
   * <p>
   * STEPS:
   * </p>
   * <ol>
   * <li>S1) Verify setting the management frame power level for 5 GHz Private WiFi Access Point (2) with value 0</li>
   * <li>S2) Verify connecting the WiFi Client to 5GHz Radio.</li>
   * <li>S3) Verify the internet connection in the Connected Client</li>
   * <li>S4) Verify the quality of the internet connection.</li>
   * <li>S5) Verify disconnecting the WiFi Client</li>
   * <li>S6) Verify setting the management frame power level for 5GHz Private WiFi Access Point (2) with value -20
   * </li>
   * <li>S7) Verify connecting the WiFi Client to 5GHz Radio.</li>
   * <li>S8) Verify the internet connection in the Connected Client</li>
   * <li>S9) Verify the quality of the internet connection does not deteriorate on changing the management frame power
   * level.</li>
   * <li>S10) Verify disconnecting the WiFi Client.</li>
   * </ol>
   *
   * @author BALAJI V
   * @refactor Govardhan
   * 
   * @param device
   *            {@link Dut}
   */
  @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
  @TestDetails(testUID = "TC-RDKB-WIFI-5054")
  public void testMgmtFramePwrControlFor5GHzRadio(Dut device) {
	String testCaseId = "TC-RDKB-WIFI-554";
	int stepNumber = 1;
	boolean result = false;
	String step = null;
	String errorMessage = null;
	String webPaParameterName = BroadBandManagementPowerControlEnum.MGMT_PWR_CTRL_WIFI_AP_9
		.getWebPaParamMgmtPower();
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-5054");
	    LOGGER.info("TEST DESCRIPTION: Management Frame Power control for 5GHz");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info(
		    "STEP 1: Verify setting the management frame power level for 5 GHz Private WiFi Access Point (2) to value '0'");
	    LOGGER.info("STEP 2: Verify connecting the WiFi Client to 5GHz Radio.");
	    LOGGER.info("STEP 3: Verify the internet connection in the Connected Client.");
	    LOGGER.info("STEP 4: Verify the quality of the internet connection.");
	    LOGGER.info("STEP 5:Verify disconnecting the WiFi Client from 5GHz Radio.");
	    LOGGER.info(
		    "STEP 6: Verify setting the management frame power level for 5 GHz Private WiFi Access Point (2) to value '-20'");
	    LOGGER.info("STEP 7: Verify connecting the WiFi Client to 5GHz Radio.");
	    LOGGER.info("STEP 8: Verify the internet connection in the Connected Client.");
	    LOGGER.info(
		    "STEP 9: Verify the quality of the internet connection does not deteriorate on changing the management frame power level.");
	    LOGGER.info("STEP 10:Verify disconnecting the WiFi Client from 5GHz Radio.");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1 : Verify setting the management frame power level for 5GHz Private WiFi Access Point (2) with
	     * value 0.
	     */
	    step = "S" + stepNumber;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE TO '0'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE TO '0' USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_ManagementFramePowerControl'.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE MUST BE SET TO '0' SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE TO '0'";
	    String expectedValue = "0";
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webPaParameterName,
		    BroadBandTestConstants.CONSTANT_1, expectedValue);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result
			    ? "SUCCESSFULLY SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE TO '0'"
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 2 : Verify connecting the WiFi Client to 5GHz Radio.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    // Connected client instance
	    Dut connectedClientSettop = null;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT TO 5GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WIFI CLIENT TO 5GHz RADIO.");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED :  WIFI CLIENT MUST BE CONNECTED TO 5GHz RADIO SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO CONNECT THE WIFI CLIENT TO 5GHz RADIO.";
	    connectedClientSettop = BroadBandConnectedClientUtils
		    .get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
	    result = null != connectedClientSettop;
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "WIFI CLIENT CONNECTED TO 5GHz RADIO SUCCESSFULLY." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 3 : Verify the internet connection in the Connected Client
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :  VERIFY THE INTERNET CONNECTION IN THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : EXECUTE COMMAND WINDOWS : ping www.google.com -n 1 | grep 'Reply from' or LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' ON THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  CONNECTED CLIENT SHOULD BE ABLE TO ACCESS THE INTERNET SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO ACCESS THE INTERNET FROM THE CONNECTED CLIENT.";
	    result = ConnectedNattedClientsUtils.verifyNetworkConnection(connectedClientSettop, tapEnv, true);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "CONNECTED CLIENT IS ABLE TO ACCESS THE INTERNET SUCCESSFULLY." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 4 : Verify the quality of the internet connection.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    int avgResponseTimeInitial = 0;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :   VERIFY THE QUALITY OF THE INTERNET CONNECTION IN THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : RETRIEVE THE AVERAGE TIME TAKEN TO ACCESS 'www.google.com' USING PING COMMAND.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED :  THE AVERAGE RESPONSE TIME MUST BE AVAILABLE.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO RETRIEVE THE AVERAGE TIME TAKEN TO PING 'www.google.com'";
	    avgResponseTimeInitial = ConnectedNattedClientsUtils.getPingResponseAvgTime(connectedClientSettop, tapEnv,
		    BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS);
	    result = (avgResponseTimeInitial > 0);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result
			    ? "SUCCESSFULLY RETRIEVED THE AVERAGE TIME TAKEN TO ACCESS 'www.google.com'. THE AVERAGE RESPONSE TIME : "
				    + avgResponseTimeInitial + " milli seconds."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 5 : Verify disconnecting the WiFi Client from 5GHz Radio.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");

	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :  VERIFY DISCONNECTING THE WIFI CLIENT FROM 5GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : DISCONNECT THE WIFI CLIENT FROM 5GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  WIFI CLIENT MUST BE DISCONNECTED FROM 5GHZ RADIO SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    // String to store the ssid
	    String ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    errorMessage = "UNABLE TO RETRIEVE 5GHz WIFI NETWORK SSID FROM THE ROUTER DEVICE.";
	    if (CommonMethods.isNotNull(ssid)) {
		errorMessage = "UNABLE TO DISCONNECT WIFI CLIENT FROM 5GHz RADIO CONNECTION.";
		result = ConnectedNattedClientsUtils.disconnectSSID(connectedClientSettop, tapEnv, ssid);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "SUCCESSFULLY DISCONNECTED THE WIFI CLIENT FROM 5GHz RADIO CONNECTION." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * STEP 6 : Verify setting the management frame power level for 5 GHz Private WiFi Access Point (2) with
	     * value -20
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : VERIFY SETTING THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE TO '-20'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE TO '-20' USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_ManagementFramePowerControl'.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE MUST BE SET TO '-20' SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    expectedValue = "-20";
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webPaParameterName,
		    BroadBandTestConstants.CONSTANT_1, expectedValue);
	    errorMessage = "UNABLE TO SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WI-FI ACCESS POINT VALUE TO '-20'";
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result
			    ? "SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WI-FI ACCESS POINT VALUE TO '-20' SUCCESSFULLY."
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 7 : Verify connecting the WiFi Client to 5GHz Radio.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    // Connected client instance
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT TO 5GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WIFI CLIENT TO 5GHz RADIO.");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED :  WIFI CLIENT MUST BE CONNECTED TO 5GHz RADIO SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO CONNECT THE WIFI CLIENT TO 5GHz RADIO.";
	    connectedClientSettop = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    result = null != connectedClientSettop;
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "WIFI CLIENT CONNECTED TO 5GHz RADIO SUCCESSFULLY." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 8 : Verify the internet connection in the Connected Client
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :  VERIFY THE INTERNET CONNECTION IN THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : EXECUTE COMMAND WINDOWS : ping www.google.com -n 1 | grep 'Reply from' or LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' ON THE CONNECTED CLIENT.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  CONNECTED CLIENT SHOULD BE ABLE TO ACCESS THE INTERNET SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO ACCESS THE INTERNET FROM THE CONNECTED CLIENT.";
	    result = ConnectedNattedClientsUtils.verifyNetworkConnection(connectedClientSettop, tapEnv, true);
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result ? "CONNECTED CLIENT IS ABLE TO ACCESS THE INTERNET SUCCESSFULLY." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    /**
	     * STEP 9 : Verify the quality of the internet connection does not deteriorate on changing the management
	     * frame power level.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    int avgResponseTimeFinal = 0;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :   VERIFY THE QUALITY OF THE INTERNET CONNECTION IN THE CONNECTED CLIENT ON CHANGING THE MANAGEMENT FRAME POWER LEVEL.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : RETRIEVE THE AVERAGE TIME TAKEN TO ACCESS 'www.google.com' USING PING COMMAND.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  THE AVERAGE RESPONSE TIME MUST BE AVAILABLE AND IT MUST NOT CHANGE DRASTICALLY");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO RETRIEVE THE AVERAGE TIME TAKEN TO PING 'www.google.com'";
	    avgResponseTimeFinal = ConnectedNattedClientsUtils.getPingResponseAvgTime(connectedClientSettop, tapEnv,
		    BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS);
	    result = avgResponseTimeFinal > 0;
	    if (result) {
		errorMessage = "UNABLE TO VERIFY THE QUALITY OF THE INTERNET CONNECTION DOES NOT DEGRADE ON CHANGING THE MGMT FRAME POWER LEVEL.PING AVG RESPONSE TIME (DEFAULT) = "
			+ avgResponseTimeInitial + ", PING AVG RESPONSE TIME (AFTER CHANGING POWER LEVEL) = "
			+ avgResponseTimeFinal;
		result = (avgResponseTimeFinal
			- avgResponseTimeInitial) <= BroadBandTestConstants.PING_RESPONSE_AVG_TIME_THRESHOLD;
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (result
			    ? "SUCCESSFULLY VERIFIED THE QUALITY OF INTERNET CONNECTION DOES NOT DEGRADE ON CHANGING THE MGMT FRAME POWER LEVEL. PING AVG RESPONSE TIME (DEFAULT) = "
				    + avgResponseTimeInitial
				    + ", PING AVG RESPONSE TIME (AFTER CHANGING POWER LEVEL) = " + avgResponseTimeFinal
			    : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * STEP 10 : Verify disconnecting the WiFi Client from 5GHz Radio.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :  VERIFY DISCONNECTING THE WIFI CLIENT FROM 5GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : DISCONNECT THE WIFI CLIENT FROM 5GHz RADIO.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  WIFI CLIENT MUST BE DISCONNECTED FROM 2.4GHZ RADIO SUCCESSFULLY.");
	    LOGGER.info("#######################################################################################");
	    // String to store the ssid
	    ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    errorMessage = "UNABLE TO RETRIEVE 5GHz WIFI NETWORK SSID FROM THE ROUTER DEVICE.";
	    if (CommonMethods.isNotNull(ssid)) {
		errorMessage = "UNABLE TO DISCONNECT WIFI CLIENT FROM 5GHz RADIO CONNECTION.";
		result = ConnectedNattedClientsUtils.disconnectSSID(connectedClientSettop, tapEnv, ssid);
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + (result
		    ? "SUCCESSFULLY DISCONNECTED THE WIFI CLIENT FROM 2.4GHz RADIO CONNECTION." : errorMessage));
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE VALIDATING THE QUALITY OF THE LINK AFTER SETTING THE 5GHz MGMT FRAME POWER LEVEL: "
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info(
		    "POST-CONDITION : STEP 1 : SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE TO '0' USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_ManagementFramePowerControl'.");
	    errorMessage = "UNABLE TO RESET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WIFI ACCESS POINT VALUE TO '0'";
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webPaParameterName,
		    BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_ZERO);
	    LOGGER.info(
		    "SET THE MANAGEMENT FRAME POWER LEVEL FOR 5GHz PRIVATE WI-FI ACCESS POINT VALUE TO '0': " + result);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    LOGGER.info("COMPLETED TEST CASE: TC-RDKB-WIFI-5054");
	    LOGGER.info("#######################################################################################");
	}
  }


}
