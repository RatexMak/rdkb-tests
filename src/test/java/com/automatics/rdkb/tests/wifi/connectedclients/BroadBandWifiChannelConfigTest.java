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
package com.automatics.rdkb.tests.wifi.connectedclients;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandWiFiPacketCaptureUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.utils.CommonMethods;

public class BroadBandWifiChannelConfigTest  extends AutomaticsTestBase {
	
    /** Instance variable to hold the status of capturing WiFi Packets */
    private boolean startedCapturingWiFiPackets = false;
    /** Instance variable to hold the Channel Number information */
    private String channelNumber = null;
    /** Instance variable to hold the Ethernet Client */
    private Dut ethernetClient = null;
	
    /**
     * Verify Client Connectivity to 2.4GHz bands with Channel Selection Mode is Manual.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify the default channel selection mode is Auto.</li>
     * <li>S2) Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID</li>
     * <li>S3) Verify the internet connectivity in the Client.</li>
     * <li>S4) Verify changing the channel selection mode to Manual.</li>
     * <li>S5) Verify the WiFi Client connected is in disconnected state.</li>
     * <li>S6) Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID</li>
     * <li>S7) Verify the internet connectivity in the Client after changing the channel selection mode.</li>
     * <li>S8) Verify disconnecting the WiFi Client from the Gateway device.</li>
     * <li>S9) Verify changing the channel value to 7.</li>
     * <li>S10) Verify retrieving the changed channel value via WebPA</li>
     * <li>S11) Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID and start capturing WiFi Packets.</li>
     * <li>S12) Verify the internet connectivity in the Client after changing the channel.</li>
     * <li>S13) Verify the Channel from WiFi Packet Capture.</li>
     * <li>S14) Verify disconnecting the WiFi Client from the Gateway device.</li>
     * <li>S15) Verify changing the channel value to 11.</li>
     * <li>S16) Verify retrieving the changed channel value via WebPA</li>
     * <li>S17) Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID and start capturing WiFi Packets.</li>
     * <li>S18) Verify the internet connectivity in the Client after changing the channel.</li>
     * <li>S19) Verify the Channel from WiFi Packet Capture.</li>
     * <li>S20) Verify disconnecting the WiFi Client from the Gateway device.</li>
     * </ol>
     * 
     * @param device
     * 
     * @author BALAJI V
	 * @refactor Athira
     */
    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-CHNL-CONFIG-5072")
    public void testChannelSelectionModeWifiRadio2Ghz(Dut device) {
	String testCaseId = "TC-RDKB-WIFI-CHNL-CONFIG-572";
	String errorMessage = null;
	boolean status = false;
	Dut wifiClientDevice = null;
	int stepNumber = 1;
	String testStepNumber = "S" + stepNumber;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CHNL-CONFIG-5072");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify Client Connectivity to 2.4GHz bands with Channel Selection Mode is Manual");
	    LOGGER.info("STEP 1: Verify the default channel selection mode is Auto.");
	    LOGGER.info("STEP 2: Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID");
	    LOGGER.info("STEP 3: Verify the internet connectivity in the Client.");
	    LOGGER.info("STEP 4: Verify changing the channel selection mode to Manual.");
	    LOGGER.info("STEP 5: Verify the WiFi Client connected is in disconnected state");
	    LOGGER.info("STEP 6: Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID");
	    LOGGER.info("STEP 7: Verify the internet connectivity in the Client.");
	    LOGGER.info("STEP 8: Verify disconnecting the WiFi Client from the Gateway device.");
	    LOGGER.info("STEP 9: Verify changing the channel value to 7.");
	    LOGGER.info("STEP 10: Verify retrieving the changed channel value via WebPA");
	    LOGGER.info("STEP 11: Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID");
	    LOGGER.info("STEP 12: Verify the internet connectivity in the Client.");
	    LOGGER.info("STEP 13: Verify the Channel from WiFi Packet Capture.");
	    LOGGER.info("STEP 14: Verify disconnecting the WiFi Client from the Gateway device.");
	    LOGGER.info("STEP 15: Verify changing the channel value to 11.");
	    LOGGER.info("STEP 16: Verify retrieving the changed channel value via WebPA");
	    LOGGER.info("STEP 17: Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID");
	    LOGGER.info("STEP 18: Verify the internet connectivity in the Client.");
	    LOGGER.info("STEP 19: Verify the Channel from WiFi Packet Capture.");
	    LOGGER.info("STEP 20: Verify disconnecting the WiFi Client from the Gateway device.");
	    LOGGER.info("#######################################################################################");

	    /**
	     * S1) Verify the default channel selection mode is Auto.
	     */
	    LOGGER.info("######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE DEFAULT CHANNEL SELECTION MODE IS AUTO.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : RETRIEVE DEFAULT CHANNEL SELECTION MODE USING WEBPA PARAM 'Device.WiFi.Radio.10000.AutoChannelEnable'");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED: THE VALUE MUST BE 'TRUE'");
	    LOGGER.info("######################################################################################");
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ,
			BroadBandTestConstants.TRUE);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (!status) {
		LOGGER.info("WIFI RADIO 2.4GHz CHANNEL SELECTION MODE IS NOT AUTO; HENCE SETTING IT AS 'AUTO'.");
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ,
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "DEFAULT CHANNEL SELECTION MODE IS 'AUTO'." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * S2) Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to connect the associated connected client to 2.4GHz SSID";
	    try {
		wifiClientDevice = BroadBandConnectedClientUtils
			.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = wifiClientDevice != null;
	    LOGGER.info(
		    "STEP " + stepNumber + " - ACTUAL: "
			    + (status
				    ? "SUCCESSFULLY CONNECTED THE WI-FI CLIENT ASSOCIATED WITH THE SETTOP TO A 2.4GHz SSID"
				    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * S3) Verify the internet connectivity in the Client.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE ping COMMAND ON THE CONNECTED CLIENT");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : INTERNET CONNECTIVITY MUST BE AVAILABLE IN THE WI-FI CLIENT.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Connected client is not able to access the site : 'www.google.com'";
	    status = ConnectedNattedClientsUtils.verifyPingConnection(wifiClientDevice, tapEnv,
		    BroadBandTestConstants.PING_TO_GOOGLE);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "CLIENT HAS INTERNET CONNECTIVITY." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * S4) Verify changing the channel selection mode to Manual.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + " : DESCRIPTION : VERIFY CHANGING THE CHANNEL SELECTION MODE TO 'MANUAL'");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : SET THE CHANNEL SELECTION MODE TO 'FALSE' USING WEBPA PARAM 'Device.WiFi.Radio.10000.AutoChannelEnable'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL SELECTION MODE MUST BE CHANGED TO 'MANUAL'");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to set the channel selection mode to 'Manual'";
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
	    LOGGER.info("Going to wait for 2 min for the channel selection mode changes to reflect");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "SUCCESSFULLY CHANGED THE CHANNEL SELECTION MODE TO 'MANUAL'" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * S5) Verify the WiFi Client connected is in disconnected state.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    String ssid = null;
	    String response = null;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : VERIFY THE WI-FI CONNECTED CLIENT IS IN DISCONNECTED STATE.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : EXECUTE THE COMMAND nmcli WINDOWS, ipconfig LINUX TO RETRIEVE THE CONNECTION STATE");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : THE WI-FI CONNECTED CLIENT SHOULD BE IN DISCONNECTED STATE");
	    LOGGER.info("#######################################################################################");
	    try {
			errorMessage = "The Wifi connected client is still connected to the 5GHz SSID";
			ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);

			if (DeviceModeHandler.isRPIDevice(device)) {
				response = tapEnv.executeCommandOnOneIPClients(wifiClientDevice,
						BroadBandCommandConstants.CMD_WINDOWS_SHOW_INTERFACES);
				if (response.contains(ssid)) {
					status = !response.contains(BroadBandTestConstants.STRING_CONSTANT_CONNECTED);
				} else {
					status = true;
				}
			} else {
				status = ConnectedNattedClientsUtils.verifyConnectToSSID(wifiClientDevice, tapEnv, ssid, false);
			}
			
	    } catch (TestException exp) {
		errorMessage = exp.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "THE CONNECTED CLIENT IS DISCIONNECTED FROM 5GHz SSID AS EXPECTED" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * S6) Verify connecting the WiFi Client in the Setup to the 2.4 GHz SSID 
	     * S7) Verify the internet connectivity in the Client after changing the channel selection mode. 
	     * S8) Verify disconnecting the WiFi Client from the Gateway device.
	     */
	    stepNumber++;
	    connectClientAndVerifyChannel(tapEnv, device, testCaseId, stepNumber, true, false);

	    /**
	     * S9) Verify changing the channel value to 7.
	     */
	    stepNumber = stepNumber + 3;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY SETTING THE CHANNEL VALUE TO 7");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : SET THE CHANNEL VALUE TO '7' USING WEBPA PARAM 'Device.WiFi.Radio.10000.Channel'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL VALUE MUST BE CHANGED TO '7'");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to set the channel value to '7'";
	    channelNumber = "7";
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ,
		    BroadBandTestConstants.CONSTANT_2, channelNumber);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "CHANNEL VALUE (2.4GHz) CHANGED TO '7' SUCCESSFULLY" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * S10) Verify retrieving the changed Channel Number
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("Going to wait for 2 min for the channel change to reflect");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY RETRIEVING THE CHANGED CHANNEL NUMBER.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : RETRIEVE THE VALUE OF WEBPA PARAM 'Device.WiFi.Radio.10000.Channel'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL VALUE MUST BE '7'");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to verify the channel value set to '7'";
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ, channelNumber);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "VERIFIED THE CHANNEL VALUE (2.4GHz) SET TO '7' SUCCESSFULLY" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * S11) Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID & start capturing WiFi Packets.
	     * S12) Verify the internet connectivity in the Client after changing the channel. 
	     * S13) Verify disconnecting the WiFi Client from the Gateway device.
	     */
	    stepNumber++;
	    connectClientAndVerifyChannel(tapEnv, device, testCaseId, stepNumber, true, false);

	    /**
	     * S14) Verify changing the channel value to 11.
	     */
	    stepNumber = stepNumber + 3;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY SETTING THE CHANNEL VALUE TO 11");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : SET THE CHANNEL VALUE TO '11' USING WEBPA PARAM 'Device.WiFi.Radio.10000.Channel'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : CHANNEL VALUE MUST BE SET TO '11'");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to set the channel value to '11'";
	    channelNumber = "11";
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ,
		    BroadBandTestConstants.CONSTANT_2, channelNumber);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "CHANNEL VALUE (2.4GHz) CHANGED TO '11' SUCCESSFULLY" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * S15) Verify retrieving the changed Channel Number
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("Going to wait for 2 min for the channel change to reflect");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY RETRIEVING THE CHANGED CHANNEL NUMBER.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : RETRIEVE THE VALUE OF WEBPA PARAM 'Device.WiFi.Radio.10000.Channel'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL VALUE MUST BE '11'");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to verify the channel value set to '11'";
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ, channelNumber);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "VERIFIED THE CHANNEL VALUE (2.4GHz) SET TO '11' SUCCESSFULLY" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * S16) Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID & start capturing WiFi Packets.
	     * S17) Verify the internet connectivity in the Client after changing the channel. 
	     * S18) Verify disconnecting the WiFi Client from the Gateway device.
	     */
	    stepNumber++;
	    connectClientAndVerifyChannel(tapEnv, device, testCaseId, stepNumber, true, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING 2.4GHz WIFI RADIO WITH CHANNEL SELECTION MODE AS MANUAL: "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
		    errorMessage, true);
	} finally {
	    executePostCondition(device, ethernetClient, true);
	    LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CHNL-CONFIG-5072");
	    LOGGER.info("#######################################################################################");
	}
    }
    
    /**
     * Verify Client Connectivity to 5GHz bands with Channel Selection Mode is Manual.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify the default channel selection mode is Auto.</li>
     * <li>S2) Verify connecting the WiFi Client in the Setup to the 5GHz SSID</li>
     * <li>S3) Verify the internet connectivity in the Client.</li>
     * <li>S4) Verify changing the channel selection mode to Manual.</li>
     * <li>S5) Verify the WiFi Client connected is in disconnected state.</li>
     * <li>S6) Verify connecting the WiFi Client in the Setup to the 5GHz SSID</li>
     * <li>S7) Verify the internet connectivity in the Client after changing the channel selection mode.</li>
     * <li>S8)Verify disconnecting the WiFi Client from the Gateway device.</li>
     * <li>S9) Verify changing the channel value to 48.</li>
     * <li>S10) Verify retrieving the changed channel value via WebPA</li>
     * <li>S11) Verify connecting the WiFi Client in the Setup to the 5GHz SSID</li>
     * <li>S12) Verify the internet connectivity in the Client after changing the channel.</li>
     * <li>S13) Verify disconnecting the WiFi Client from the Gateway device.</li>
     * <li>S14) Verify changing the channel value to 128.</li>
     * <li>S15) Verify retrieving the changed channel value via WebPA</li>
     * <li>S16) Verify connecting the WiFi Client in the Setup to the 5GHz SSID</li>
     * <li>S17) Verify the internet connectivity in the Client after changing the channel.</li>
     * <li>S18) Verify disconnecting the WiFi Client from the Gateway device.</li>
     * </ol>
     * 
     * @param device
     * 
     * @author BALAJI V
	 * @refactor Athira
     */
    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-CHNL-CONFIG-5073")
    public void testChannelSelectionModeWifiRadio5Ghz(Dut device) {
	String testCaseId = "TC-RDKB-WIFI-CHNL-CONFIG-573";
	String errorMessage = null;
	boolean status = false;
	Dut wifiClientDevice = null;
	int stepNumber = 1;
	String testStepNumber = "S" + stepNumber;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CHNL-CONFIG-5073");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify Client Connectivity to 5GHz band with Channel Selection Mode is Manual");
	    LOGGER.info("STEP 1: Verify the default channel selection mode is Auto.");
	    LOGGER.info("STEP 2: Verify connecting the WiFi Client in the Setup to the 5GHz SSID");
	    LOGGER.info("STEP 3: Verify the internet connectivity in the Client.");
	    LOGGER.info("STEP 4: Verify changing the channel selection mode to Manual.");
	    LOGGER.info("STEP 5: Verify the WiFi Client connected is in disconnected state");
	    LOGGER.info("STEP 6: Verify connecting the WiFi Client in the Setup to the 5GHz SSID");
	    LOGGER.info("STEP 7: Verify the internet connectivity in the Client.");
	    LOGGER.info("STEP 8: Verify disconnecting the WiFi Client from the Gateway device.");
	    LOGGER.info("STEP 9: Verify changing the channel value to 48.");
	    LOGGER.info("STEP 10: Verify retrieving the changed channel value via WebPA.");
	    LOGGER.info("STEP 11: Verify connecting the WiFi Client in the Setup to the 5GHz SSID");
	    LOGGER.info("STEP 12: Verify the internet connectivity in the Client after changing the channel.");
	    LOGGER.info("STEP 13: Verify disconnecting the WiFi Client from the Gateway device.");
	    LOGGER.info("STEP 14: Verify changing the channel value to 128.");
	    LOGGER.info("STEP 15: Verify retrieving the changed channel value via WebPA.");
	    LOGGER.info("STEP 16: Verify connecting the WiFi Client in the Setup to the 5GHz SSID");
	    LOGGER.info("STEP 17: Verify the internet connectivity in the Client after changing the channel.");
	    LOGGER.info("STEP 18: Verify disconnecting the WiFi Client from the Gateway device.");
	    LOGGER.info("#######################################################################################");

	    /**
	     * S1) Verify the default channel selection mode is Auto.
	     */
	    LOGGER.info("######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE DEFAULT CHANNEL SELECTION MODE IS AUTO.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : RETRIEVE DEFAULT CHANNEL SELECTION MODE USING WEBPA PARAM 'Device.WiFi.Radio.10100.AutoChannelEnable'");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED: THE VALUE MUST BE 'TRUE'");
	    LOGGER.info("######################################################################################");
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ,
			BroadBandTestConstants.TRUE);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (!status) {
		LOGGER.info("WIFI RADIO 5GHz CHANNEL SELECTION MODE IS NOT AUTO; HENCE SETTING IT AS 'AUTO'.");
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ,
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "THE DEFAULT CHANNEL SELECTION MODE IS 'AUTO' AS EXPECTED." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * S2) Verify connecting the WiFi Client in the Setup to the 5GHz SSID
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT TO 5GHz SSID");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT TO 5GHz SSID AND PASSWORD");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED: WI-FI CLIENT MUST BE CONNECTED TO THE 5GHz SSID.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to connect the associated connected client to 5GHz SSID";
	    try {
		wifiClientDevice = BroadBandConnectedClientUtils
			.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = wifiClientDevice != null;
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "SUCCESSFULLY CONNECTED THE WI-FI CLIENT TO 5GHz SSID" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * S3) Verify the internet connectivity in the Client.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE ping COMMAND ON THE CONNECTED CLIENT");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Connected client not able to access the site : 'www.google.com' after changing the Channel.";
	    status = ConnectedNattedClientsUtils.verifyPingConnection(wifiClientDevice, tapEnv,
		    BroadBandTestConstants.PING_TO_GOOGLE);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "CLIENT HAS INTERNET CONNECTIVITY." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * S4) Verify changing the channel selection mode to Manual.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + " : DESCRIPTION : VERIFY CHANGING THE CHANNEL SELECTION MODE TO 'MANUAL'");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : SET THE CHANNEL SELECTION MODE TO 'FALSE' USING WEBPA PARAM 'Device.WiFi.Radio.10100.AutoChannelEnable'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL SELECTION MODE MUST BE CHANGED TO 'MANUAL'");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to change the channel selection mode to 'Manual'";
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
	    LOGGER.info("Going to wait for 2 min for the channel selection mode changes to reflect");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "SUCCESSFULLY CHANGED THE CHANNEL SELECTION MODE TO 'MANUAL'" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * S5) Verify the WiFi Client connected is in disconnected state.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    String ssid = null;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : VERIFY THE WI-FI CONNECTED CLIENT IS IN DISCONNECTED STATE.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : EXECUTE THE COMMAND nmcli WINDOWS, ipconfig LINUX TO RETRIEVE THE CONNECTION STATE.");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : THE WI-FI CONNECTED CLIENT SHOULD BE IN DISCONNECTED STATE");
	    LOGGER.info("#######################################################################################");
	    try {
		errorMessage = "The Wifi connected client is still connected to the 5GHz SSID";
		ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_5_GHZ);
		status = ConnectedNattedClientsUtils.verifyConnectToSSID(wifiClientDevice, tapEnv, ssid, false);
		LOGGER.info("STEP 5 Is Wifi connected client is still connected to the 5GHz SSID " + status);
		boolean status1 = false;
	    status1 = ConnectedNattedClientsUtils.verifyPingConnection(wifiClientDevice, tapEnv,
			    BroadBandTestConstants.PING_TO_GOOGLE);
	    LOGGER.info("STEP 5  Is PING conn. status is  " + status1);
	    } catch (TestException exp) {
		errorMessage = exp.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "THE CONNECTED CLIENT IS DISCIONNECTED FROM 5GHz SSID AS EXPECTED" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * S6) Verify connecting the WiFi Client in the Setup to the 5GHz SSID 
	     * S7) Verify the internet connectivity in the Client. 
	     * S8) Verify disconnecting the WiFi Client from the Gateway device.
	     */
	    stepNumber++;
	    connectClientAndVerifyChannel(tapEnv, device, testCaseId, stepNumber, false, false);

	    /**
	     * S9) Verify changing the channel to a random values from the possible channel values.
	     */
	    stepNumber = stepNumber + 3;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    channelNumber = getRandomChannelNumber(tapEnv, device, true);
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY SETTING THE CHANNEL VALUE TO " + channelNumber);
	    LOGGER.info("STEP " + stepNumber + " : ACTION : SET THE CHANNEL VALUE TO " + channelNumber
		    + " USING WEBPA PARAM 'Device.WiFi.Radio.10100.Channel'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL VALUE MUST BE CHANGED TO " + channelNumber);
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to set the channel value to " + channelNumber;
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
		    BroadBandTestConstants.CONSTANT_2, channelNumber);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "CHANNEL VALUE (5GHz) CHANGED SUCCESSFULLY TO " + channelNumber : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * S10) Verify retrieving the changed Channel Number
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("Going to wait for 2 min for the channel change to reflect");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY RETRIEVING THE CHANGED CHANNEL NUMBER.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : RETRIEVE THE VALUE OF WEBPA PARAM 'Device.WiFi.Radio.10100.Channel'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL VALUE MUST BE " + channelNumber);
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to verify the channel value set to " + channelNumber;
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ, channelNumber);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + (status
		    ? "VERIFIED THE CHANNEL VALUE (5GHz) SET SUCCESSFULLY TO " + channelNumber : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * S11) Verify connecting the WiFi Client in the Setup to the 5GHz SSID & start capturing WiFi Packets. 
	     * S12) Verify the internet connectivity in the Client after changing the channel. 
	     * S13) Verify disconnecting the WiFi Client from the Gateway device.
	     */
	    stepNumber++;
	    connectClientAndVerifyChannel(tapEnv, device, testCaseId, stepNumber, false, false);

	    /**
	     * S14) Verify changing the channel to a random values from the possible channel values (other than the
	     * previous one).
	     */
	    stepNumber = stepNumber + 3;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    channelNumber = getRandomChannelNumber(tapEnv, device, true);
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY SETTING THE CHANNEL VALUE TO " + channelNumber);
	    LOGGER.info("STEP " + stepNumber + " : ACTION : SET THE CHANNEL VALUE TO " + channelNumber
		    + " USING WEBPA PARAM 'Device.WiFi.Radio.10100.Channel'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL VALUE MUST BE CHANGED TO " + channelNumber);
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to verify the channel value set to " + channelNumber;
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
		    BroadBandTestConstants.CONSTANT_2, channelNumber);
	    
		String response1 = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
		
		LOGGER.info("STEP 14 channel number retrived" + response1);
	    
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "CHANNEL VALUE (5GHz) CHANGED SUCCESSFULLY TO " + channelNumber : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * S15) Verify retrieving the changed Channel Number
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("Going to wait for 2 min for the channel change to reflect");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY RETRIEVING THE CHANGED CHANNEL NUMBER.");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : RETRIEVE THE VALUE OF WEBPA PARAM 'Device.WiFi.Radio.10100.Channel'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL VALUE MUST BE " + channelNumber);
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to verify the channel value set to " + channelNumber;
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ, channelNumber);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + (status
		    ? "VERIFIED THE CHANNEL VALUE (5GHz) SET SUCCESSFULLY TO " + channelNumber : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * S16) Verify connecting the WiFi Client in the Setup to the 5GHz SSID & start capturing WiFi Packets. 
	     * S17) Verify the internet connectivity in the Client after changing the channel. 
	     * S18) Verify disconnecting the WiFi Client from the Gateway device.
	     */
	    stepNumber++;
	    connectClientAndVerifyChannel(tapEnv, device, testCaseId, stepNumber, false, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING 5GHz WIFI RADIO WITH CHANNEL SELECTION MODE AS MANUAL: "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
		    errorMessage, true);
	} finally {
	    executePostCondition(device, ethernetClient, false);
	    LOGGER.info("COMPLETED TEST CASE: TC-RDKB-WIFI-CHNL-CONFIG-5073");
	    LOGGER.info("#######################################################################################");
	}
    }
    
    /**
     * Helper Method to execute the test steps which are common across various test cases. It covers the test steps that
     * are getting repeated; it includes connecting the wifi client, packet capture, verify internet connectivity, and
     * disconnect the wifi client.
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi}
     * @param device
     *            {@link Dut}
     * @param testCaseId
     *            String representing the Test Case ID.
     * @param stepNumber
     *            Integer representing the step number.
     * @param verify2GhzRadio
     *            Boolean flag representing the verification radio, TRUE for 2.4GHz; FALSE for 5GHz.
     * @param captureWiFiPackets
     *            Boolean flag representing captureWiFiPackets needed or not.  
     * @refactor Athira        
     */
    private void connectClientAndVerifyChannel(AutomaticsTapApi tapEnv, Dut device, String testCaseId, int stepNumber,
	    boolean verify2GhzRadio, boolean captureWiFiPackets) {
	boolean status = false;
	String errorMessage = null;
	Dut wifiClientDevice = null;
	String ssid = null;
	BroadBandResultObject resultObject = null;
	String testStepNumber = "S" + stepNumber;
	String wifiRadio = verify2GhzRadio ? BroadBandTestConstants.BAND_2_4GHZ : BroadBandTestConstants.BAND_5GHZ;
	LOGGER.info("WIFI RADIO TO BE VALIDATED: " + wifiRadio);

	try {
	    /**
	     * Verify connecting the WiFi Client in the Setup to the 2.4GHz/5GHz SSID & start capturing WiFi Packets as
	     * required.
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + ": VERIFY THE CONNECTING THE WIFI CLIENT TO THE GATEWAY DEVICE.");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH " + wifiRadio
		    + " SSID AND PASSWORD");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : WIFI CLIENT MUST BE CONNECTED TO THE GATEWAY DEVICE.");
	    LOGGER.info("#######################################################################################");
	    if (captureWiFiPackets) {
		errorMessage = "Unable to retrieve the Ethernet Client from the Connected Client Setup.";
		ethernetClient = BroadBandWiFiPacketCaptureUtils.getEthernetConnectedLinuxClient(tapEnv, device);
		boolean isEthernetClientAvailable = null != ethernetClient;
		// Start Capturing WiFi Packets.
		if (isEthernetClientAvailable) {
		    errorMessage = "Unable to start capturing WiFi packets for verifying the channel number information.";
		    resultObject = BroadBandWiFiPacketCaptureUtils.startCapturingPackets(tapEnv, ethernetClient);
		    startedCapturingWiFiPackets = resultObject.isStatus();
		    errorMessage = resultObject.getErrorMessage();
		}
	    }
	    // Connect to WiFi Client. In case, WiFi Packet capture is required, then the packet capture initialization
	    // must be successful to connect the WiFi Client.
	    if (captureWiFiPackets ? startedCapturingWiFiPackets : true) {
		errorMessage = "Unable to connect the WiFi Client to the device with appropriate SSID & Password "
			+ wifiRadio;
		try {
		    wifiClientDevice = verify2GhzRadio
			    ? BroadBandConnectedClientUtils
				    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv)
			    : BroadBandConnectedClientUtils
				    .get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
		} catch (TestException testException) {
		    // Log & Suppress the exception
		    LOGGER.error(testException.getMessage());
		}
		status = null != wifiClientDevice;
	    }
	    // Stop Capturing the WiFi Packets.
	    if (startedCapturingWiFiPackets) {
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		resultObject = BroadBandWiFiPacketCaptureUtils.stopCapturingPackets(tapEnv, ethernetClient);
		LOGGER.info("STOPPED CAPTURING WIFI PACKETS: " + resultObject.isStatus());
	    }
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
		    + (status ? "VERIFIED CONNECTING THE WIFI CLIENT TO WIFI NETWORK SUCCESSFULLY." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Verify the internet connectivity in the Client after changing the channel.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT AFTER CHANGING THE CHANNEL.");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE ping COMMAND ON THE CONNECTED CLIENT");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Connected client not able to access the site : 'www.google.com' after changing the Channel.";
	    status = ConnectedNattedClientsUtils.verifyPingConnection(wifiClientDevice, tapEnv,
		    BroadBandTestConstants.PING_TO_GOOGLE);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "CLIENT HAS INTERNET CONNECTIVITY AFTER CHANGING THE CHANNEL." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Verify the Channel from WiFi Packet Capture.
	     */
	    if (captureWiFiPackets) {
		stepNumber++;
		testStepNumber = "S" + stepNumber;
		status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber
			+ " : DESCRIPTION : VERIFY THE CHANNEL INFORMATION FROM CAPTURED WIFI PACKETS.");
		LOGGER.info("STEP " + stepNumber
			+ ": ACTION : READ THE CAPTURED PACKETS USING TCPDUMP & CONFIRM THE CHANNEL NUMBER IS: "
			+ channelNumber);
		LOGGER.info("STEP " + stepNumber
			+ ": EXPECTED : CHANNEL INFORMATION IN CAPTURED WIFI PACKETS MUST BE SAME AS CHANNEL NUMBER SET.");
		LOGGER.info("#######################################################################################");
		errorMessage = "Unable to start capturing WiFi packets for verifying the channel number information";
		if (startedCapturingWiFiPackets) {
		    errorMessage = "Unable to verify the Channel information in captured wifi packets is same as the channel number  set: "
			    + channelNumber;
		    resultObject = BroadBandWiFiPacketCaptureUtils.searchInPcapFile(tapEnv, ethernetClient,
			    BroadBandTraceConstants.SEARCH_TEXT_CHANNEL_PCAP
				    .replace(BroadBandTestConstants.CHANNEL_NUMBER_PLACE_HOLDER, channelNumber));
		    status = resultObject.isStatus();
		    errorMessage = resultObject.getErrorMessage();
		}
		LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
			+ (status
				? "VERIFIED THE CHANNEL INFORMATION IN CAPTURED WIFI PACKETS IS SAME AS THE CHANNEL NUMBER SET: "
					+ channelNumber
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
	    }

	    /**
	     * Verify disconnecting the WiFi Client from the Gateway device.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT FROM "
		    + wifiRadio + " SSID");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT FROM " + wifiRadio + " SSID");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED: CLIENT MUST BE DISCONNECTED FROM SSID SUCCESSFULLY");
	    LOGGER.info("#######################################################################################");
	    try {
		errorMessage = "Attempt to disconnect the " + wifiRadio + " SSID wifi connection Failed";
		ssid = verify2GhzRadio
			? BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
				WiFiFrequencyBand.WIFI_BAND_2_GHZ)
			: BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
				WiFiFrequencyBand.WIFI_BAND_5_GHZ);
		status = ConnectedNattedClientsUtils.disconnectSSID(wifiClientDevice, tapEnv, ssid);
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "SUCCESSFULLY DISCONNECTED THE CLIENT FROM " + wifiRadio + " SSID" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING " + wifiRadio + " WITH CHANNEL SELECTION MODE AS MANUAL: "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
		    errorMessage, true);
	}
    }
    
    /**
     * Helper Method for performing Post-Conditions.
     * 
     * @param device
     *            {@link Dut}
     * @param ethernetClient
     *            Dut object representing the Ethernet Client
     * @param is2GhzRadio
     *            Boolean flag representing the 2.4GHz/5GHz Radio; TRUE if 2.4GHz Radio; else FALSE
     * @param startedCapturingWiFiPackets
     *            Boolean representing the flag whether packet capture started successfully
     */
    private void executePostCondition(Dut device, Dut ethernetClient, boolean is2GhzRadio) throws TestException {
	boolean status = false;
	String errorMessage = null;
	String webpaParam = is2GhzRadio ? BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ
		: BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ;
	LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	LOGGER.info("#######################################################################################");
	LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY CHANGING THE CHANNEL SELECTION MODE TO 'AUTO'");
	LOGGER.info("POST-CONDITION 1 : ACTION : SET THE CHANNEL SELECTION MODE TO 'AUTO' USING WEBPA PARAM '"
		+ webpaParam + "'");
	LOGGER.info("POST-CONDITION 1 : EXPECTED : CHANNEL SELECTION MODE MUST BE CHANGED TO 'AUTO'");
	LOGGER.info("#######################################################################################");
	try {
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webpaParam,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	} catch (Exception exception) {
	    errorMessage = "Unable to change the channel selection mode to 'Auto' using WebPA Param " + webpaParam;
	}
	if (!status) {
	    LOGGER.info("SETTING THE CHANNEL SELECTION MODE TO 'Auto' VIA WEBPA FAILED. HENCE ATTMEPTING VIA SNMP.");
	    try {
		BroadBandSnmpMib wifiAutoChannelMib = is2GhzRadio ? BroadBandSnmpMib.ECM_WIFI_2_4_CHANNEL_INFO
			: BroadBandSnmpMib.ECM_WIFI_5_CHANNEL_INFO;
		String response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
			wifiAutoChannelMib.getOid(), SnmpDataType.UNSIGNED_INTEGER, BroadBandTestConstants.STRING_ZERO,
			wifiAutoChannelMib.getTableIndex());
		status = CommonMethods.isNotNull(response)
			&& response.trim().equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
		errorMessage = "Unable to change the channel selection mode to 'Auto' using SNMP Param "
			+ wifiAutoChannelMib.getOid();
	    } catch (Exception exception) {
		errorMessage = "Unable to change the channel selection mode to 'Auto' using SNMP.";
	    }
	}
	LOGGER.info("POST-CONDITION 1  - ACTUAL: "
		+ (status ? "SUCCESSFULLY CHANGED THE CHANNEL SELECTION MODE TO 'AUTO'" : errorMessage));
	LOGGER.info("#######################################################################################");
	LOGGER.info("POST-CONDITION 2 : DESCRIPTION : VERIFY REMOVING PACKET CAPTURE FILE.");
	LOGGER.info(
		"POST-CONDITION 2 : ACTION : REMOVE THE FILES USING COMMAND: rm -f /tmp/rdkb_test_pc.cap /root/packet_capture_script.sh.");
	LOGGER.info("POST-CONDITION 2 : EXPECTED : FILES MUST BE REMOVED.");
	LOGGER.info("#######################################################################################");
	try {
	    if (null != ethernetClient) {
		tapEnv.executeCommandOnOneIPClients(ethernetClient,
			BroadBandCommandConstants.CMD_REMOVE_WIFI_PACKET_CAPURE);
		LOGGER.info("POST-CONDITION 2  - ACTUAL: REMOVED PACKET CAPTURE FILE SUCCESSFULLY.");
	    } else {
		LOGGER.info(
			"POST-CONDITION 2  - ACTUAL: SKIPPING THIS STEP, AS PACKET CAPTURE COULD NOT BE PERFORMED.");
	    }
	} catch (Exception exception) {
	    // Log & Suppress the exception
	    LOGGER.error(exception.toString());
	}
	// Reset the flag to FALSE.
	startedCapturingWiFiPackets = false;
	LOGGER.info("#######################################################################################");
	LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
    }

    /**
     * Helper Method to retrieve the channel number in Random manner. It retrieves the possible channel list; then the
     * current channel number; then retrieves the element index randomly to return the channel number.
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi}
     * @param device
     *            {@link Dut}
     * @param isChannelFor5Ghz
     *            Boolean representing the 2.4GHz or 5GHz WiFi Band
     * 
     * @return String representing the random channel number.
     * @refactor Athira
     */
    public String getRandomChannelNumber(AutomaticsTapApi tapEnv, Dut device, boolean isChannelFor5Ghz) {
	LOGGER.debug("ENTERING METHOD getRandomChannelNumber");
	String strChannelNumber = null;
	String possibleChannelWebPaParam = isChannelFor5Ghz
		? BroadBandWebPaConstants.WEBPA_PARAM_FOR_POSSIBLECHANNELS_IN_5GHZ
		: BroadBandWebPaConstants.WEBPA_PARAM_FOR_POSSIBLECHANNELS_IN_2GHZ;
	String possibleChannels = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		possibleChannelWebPaParam);
	LOGGER.info("POSSIBLE CHANNEL VALUES: " + possibleChannels);
	String channelWebPaParam = isChannelFor5Ghz
		? BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ
		: BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ;
	String currentChannel = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		channelWebPaParam);
	LOGGER.info("CURRENT CHANNEL NUMBER: " + currentChannel);
	if (CommonMethods.isNotNull(possibleChannels) && CommonMethods.isNotNull(currentChannel)) {
	    ArrayList<String> possibleChannelList = new ArrayList<String>(
		    Arrays.asList(possibleChannels.split(AutomaticsConstants.COMMA)));
	    possibleChannelList.remove(currentChannel);
	    Random rand = new Random();
	    int randomIndex = rand.nextInt(possibleChannelList.size() - 1);
	    LOGGER.info("RANDOM INDEX: " + randomIndex);
	    strChannelNumber = possibleChannelList.get(randomIndex);
	}
	LOGGER.info("RANDOM CHANNEL NUMBER OBTAINED: " + strChannelNumber);
	LOGGER.debug("ENDING METHOD getRandomChannelNumber");
	return strChannelNumber;
    }
}
