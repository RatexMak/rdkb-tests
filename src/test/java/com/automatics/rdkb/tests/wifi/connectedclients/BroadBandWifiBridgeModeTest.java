/**
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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;

/**
 * Test class for the verification of Client Connectivity to 2.4GHz & 5GHz bands with Bridge mode is 'ON' & 'OFF'.
 * 
 * 
 */
public class BroadBandWifiBridgeModeTest extends AutomaticsTestBase {
	
	/** Enum that stores the Lan Mode values */
    public enum LanModeTypes {
	BRIDGE_STATIC("bridge-static"), ROUTER("router");

	private String lanModeValue;

	/**
	 * @return the lanModeValue
	 */
	public String getLanModeValue() {
	    return lanModeValue;
	}

	/**
	 * @param lanModeValue
	 *            the lanModeValue to set
	 */
	public void setLanModeValue(String lanModeValue) {
	    this.lanModeValue = lanModeValue;
	}

	private LanModeTypes(String value) {
	    this.setLanModeValue(value);
	}
    };



    /**
     * Test case is created as part of COVERAGE AUTOMATION 
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>1:VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID</li>
     * <li>2:VERIFY WHETHER THE WIFI CONNECTED CLIENT 1 GOT THE IPV4 ADDRESS FROM DHCP RANGE.</li>
     * <li>3:VERIFY WHETHER WIFI CONNECTED CLIENT 1 OBTAINED THE IPV6 ADDRESS.</li>
     * <li>4:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST</li>
     * <li>5:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST</li>
     * <li>6:VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 5GHz SSID</li>
     * <li>7:VERIFY WHETHER WIFI CONNECTED CLIENT 2 GOT THE IPV4 ADDRESS FROM DHCP RANGE.</li>
     * <li>8:VERIFY WHETHER WIFI CONNECTED CLIENT 2 OBTAINED THE IPV6 ADDRESS.</li>
     * <li>9:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST</li>
     * <li>10:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST</li>
     * <li>11:RETRIEVE ETHERNET CONNECTED CLIENT FROM CONNECTED CLIENTS LIST</li>
     * <li>12:VERIFY WHETHER ETHERNET CONNECTED CLIENT GOT THE IPV4 ADDRESS FROM DHCP RANGE.</li>
     * <li>13:VERIFY WHETHER ETHERNET CONNECTED CLIENT OBTAINED THE IPV6 ADDRESS.</li>
     * <li>14:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST</li>
     * <li>15:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST</li>
     * <li>16:VERIFY ENABLING BRIDGE MODE ON THE DEVICE.</li>
     * <li>17:VERIFY WEBPA PROCESS IS UP AND RUNNING</li>
     * <li>18:VERIFY THAT THE PRIVATE WI-FI SSIDS 2.4GHZ AND 5GHZ ARE DISABLED USING WEBPA</li>
     * <li>19:VERIFY 2.4GHZ SSID NAME IS NOT BROADCASTED.</li>
     * <li>20:VERIFY WIFI CONNECTED CLIENT 1 IPV4 ADDRESS NOT IN DHCP RANGE</li>
     * <li>21:VERIFY IPV6 ADDRESS NOT AVAILABLE IN WIFI CONNECTED CLIENT 1</li>
     * <li>22:VERIFY WHETHER THERE IS CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV4</li>
     * <li>23:VERIFY WHETHER YOU HAVE NO CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV6</li>
     * <li>24:VERIFY 5GHz SSID NAME IS NOT BROADCASTED.</li>
     * <li>25:VERIFY WIFI CONNECTED CLIENT 2 IPV4 ADDRESS NOT IN DHCP RANGE.</li>
     * <li>26:VERIFY IPV6 ADDRESS NOT AVAILABLE IN WIFI CONNECTED CLIENT 1</li>
     * <li>27:VERIFY WHETHER THERE IS CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV4</li>
     * <li>28:VERIFY WHETHER YOU HAVE CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV6</li>
     * <li>29:VERIFY WHETHER THE INTERFACE GET THE CORRECT IPV4 ADDRESS.</li>
     * <li>30:VERIFY WHETHER INTERFACE GET THE CORRECT IPV6 ADDRESS.</li>
     * <li>31:VERIFY WHETHER THERE IS CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV4</li>
     * <li>32:VERIFY WHETHER YOU HAVE CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV6</li>
     * <li>33:VERIFY DISABLING BRIDGE MODE ON THE DEVICE.</li>
     * <li>34:VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID WITH BRIDGE MODE DISABLED</li>
     * <li>35:VERIFY WHETHER WIFI CONNECTED CLIENT 1 GOT THE IPV4 ADDRESS FROM DHCP RANGE.</li>
     * <li>36:VERIFY WHETHER WIFI CONNECTED CLIENT 1 OBTAINED THE IPV6 ADDRESS.</li>
     * <li>37:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST</li>
     * <li>38:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST</li>
     * <li>39:VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 5GHz SSID</li>
     * <li>40:VERIFY WHETHER WIFI CONNECTED CLIENT 2 GOT THE IPV4 ADDRESS FROM DHCP RANGE.</li>
     * <li>41:VERIFY WHETHER WIFI CONNECTED CLIENT 2 OBTAINED THE IPV6 ADDRESS.</li>
     * <li>42:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST</li>
     * <li>43:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST</li>
     * <li>44:VERIFY WHETHER ETHERNET CONNECTED CLIENT GOT THE IPV4 ADDRESS FROM DHCP RANGE.</li>
     * <li>45:VERIFY WHETHER ETHERNET CONNECTED CLIENT OBTAINED THE IPV6 ADDRESS</li>
     * <li>46:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST</li>
     * <li>47:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST</li>
     * </ol>
     * 
     * @param device
     * 
     * @refactor yamini.s
     * 
     */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true)
    @TestDetails(testUID = "TC-RDKB-BRID-MODE-5001")
    public void testWifiBand2GhzBridgeMode(Dut device) {
	String testCaseId = "TC-RDKB-BRID-MODE-501";// String testcase ID
	String errorMessage = null;// String to store error message
	boolean status = false;// boolean to store status
	Dut wifiClientDevice = null;// Dut object to store wificlient
	String testStepNumber = "S1";// String to store teststep
	boolean visiblityStatus = false;// boolean to store status of wifi visibility
	boolean disabledBridgeMode = false;// Status for bridgemode disable
	Dut ethernetClient = null;// Dut object to store ethernetClient
	Dut wifi5GhzClientDevice = null;// Dut object for connected client
	boolean wifi24Status = false;// Wifi 2.4Ghz connectivity status
	boolean wifi5Status = false;// Wifi 5Ghz connectivity status
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-BRID-MODE-5001");
	    LOGGER.info("TEST DESCRIPTION: Verify device and connected client status while device is in bridge mode");
	  
	    LOGGER.info("STEP 1:VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID");
	    LOGGER.info("STEP 2:VERIFY WHETHER WIFI CONNECTED CLIENT 1 GOT THE IPV4 ADDRESS FROM DHCP RANGE.");
	    LOGGER.info("STEP 3:VERIFY WHETHER WIFI CONNECTED CLIENT 1 OBTAINED THE IPV6 ADDRESS.");
	    LOGGER.info("STEP 4:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST ");
	    LOGGER.info("STEP 5:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST");
	    LOGGER.info("STEP 6:VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 5GHz SSID");
	    LOGGER.info("STEP 7:VERIFY WHETHER THE WIFI CONNECTED CLIENT 2 GOT THE IPV4 ADDRESS FROM DHCP RANGE.");
	    LOGGER.info("STEP 8:VERIFY WHETHER WIFI CONNECTED CLIENT 2 OBTAINED THE IPV6 ADDRESS.");
	    LOGGER.info("STEP 9:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST ");
	    LOGGER.info("STEP 10:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST");
	    LOGGER.info("STEP 11:RETRIEVE ETHERNET CONNECTED CLIENT FROM CONNECTED CLIENTS LIST");
	    LOGGER.info("STEP 12:VERIFY WHETHER THE ETHERNET CONNECTED CLIENT GOT THE IPV4 ADDRESS FROM DHCP RANGE.");
	    LOGGER.info("STEP 13:VERIFY WHETHER ETHERNET CONNECTED CLIENT OBTAINED THE IPV6 ADDRESS.");
	    LOGGER.info("STEP 14:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST ");
	    LOGGER.info("STEP 15:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST");
	    LOGGER.info("STEP 16:VERIFY ENABLING BRIDGE MODE ON THE DEVICE.");
	    LOGGER.info("STEP 17:VERIFY WEBPA PROCESS IS UP AND RUNNING ");
	    LOGGER.info("STEP 18:VERIFY THAT THE PRIVATE WI-FI SSIDS 2.4GHZ AND 5GHZ ARE DISABLED USING WEBPA");
	    LOGGER.info("STEP 19:VERIFY 2.4GHZ SSID NAME IS NOT BROADCASTED.");
	    LOGGER.info("STEP 20:VERIFY WIFI CONNECTED CLIENT 1 IPV4 ADDRESS NOT IN DHCP RANGE");
	    LOGGER.info("STEP 21:VERIFY IPV6 ADDRESS NOT AVAILABLE IN WIFI CONNECTED CLIENT 1");
	    LOGGER.info("STEP 22:VERIFY WHETHER THERE IS  CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV4");
	    LOGGER.info("STEP 23:VERIFY WHETHER YOU HAVE NO CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV6");
	    LOGGER.info("STEP 24:VERIFY 5GHz SSID NAME IS NOT BROADCASTED.");
	    LOGGER.info("STEP 25:VERIFY WIFI CONNECTED CLIENT 2 IPV4 ADDRESS NOT IN DHCP RANGE.");
	    LOGGER.info("STEP 26:VERIFY IPV6 ADDRESS NOT AVAILABLE IN WIFI CONNECTED CLIENT 1");
	    LOGGER.info("STEP 27:VERIFY WHETHER THERE IS  CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV4 ");
	    LOGGER.info("STEP 28:VERIFY WHETHER YOU HAVE CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV6");
	    LOGGER.info("STEP 29:VERIFY WHETHER THE INTERFACE GET THE CORRECT IPV4 ADDRESS.");
	    LOGGER.info("STEP 30:VERIFY WHETHER INTERFACE  GET THE CORRECT IPV6  ADDRESS.");
	    LOGGER.info("STEP 31:VERIFY WHETHER THERE IS  CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV4");
	    LOGGER.info("STEP 32:VERIFY WHETHER YOU HAVE CONNECTIVITY USING THAT PARTICULAR INTERFACE USING IPV6");
	    LOGGER.info("STEP 33:VERIFY DISABLING BRIDGE MODE ON THE DEVICE.");
	    LOGGER.info(
		    "STEP 34:VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID WITH BRIDGE MODE DISABLED");
	    LOGGER.info("STEP 35:VERIFY WHETHER THE WIFI CONNECTED CLIENT 1 GOT THE IPV4 ADDRESS FROM DHCP RANGE.");
	    LOGGER.info("STEP 36:VERIFY WHETHER WIFI CONNECTED CLIENT 1 OBTAINED THE IPV6 ADDRESS.");
	    LOGGER.info("STEP 37:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST ");
	    LOGGER.info("STEP 38:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST");
	    LOGGER.info("STEP 39:VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 5GHz SSID");
	    LOGGER.info("STEP 40:VERIFY WHETHER THE WIFI CONNECTED CLIENT 2 GOT THE IPV4 ADDRESS FROM DHCP RANGE.");
	    LOGGER.info("STEP 41:VERIFY WHETHER WIFI CONNECTED CLIENT 2 OBTAINED THE IPV6 ADDRESS.");
	    LOGGER.info("STEP 42:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST ");
	    LOGGER.info("STEP 43:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST");
	    LOGGER.info("STEP 44:VERIFY WHETHER THE ETHERNET CONNECTED CLIENT GOT THE IPV4 ADDRESS FROM DHCP RANGE.");
	    LOGGER.info("STEP 45:VERIFY WHETHER ETHERNET CONNECTED CLIENT OBTAINED THE IPV6 ADDRESS.");
	    LOGGER.info("STEP 46:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST ");
	    LOGGER.info("STEP 47:VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "######################################### STARTING PRE-CONFIGURATIONS #########################################");
	    LOGGER.info("PRE-CONFIGURATIONS TEST STEPS : ");
	    LOGGER.info("1.Verify whether Private SSID 2.4Ghz can be enabled using webPA");
	    LOGGER.info("2.Verify whether Private SSID 5Ghz can be enabled using webPA");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 1: Verify whether Private SSID 2.4Ghz can be enabled using webPA");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED: Private SSID 2.4Ghz should be enabled successfully");
	    errorMessage = "Unable to enable 2.4Ghz private SSID using webpa";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (!status) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 2: Verify whether Private SSID 5Ghz can be enabled using webPA");
	    LOGGER.info("PRE-CONDITION 2: EXPECTED: Private SSID 5Ghz should be enabled successfully");
	    errorMessage = "Unable to enable 5Ghz private SSID using webpa";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (!status) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    /**
	     * STEP 1: Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID
	     */
	    testStepNumber = "S1";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 1 : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID");
	    LOGGER.info("STEP 1 : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
	    LOGGER.info("STEP 1 : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
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
	    if (status) {
		LOGGER.info(
			"STEP 1:ACTUAL :SUCCESSFULLY CONNECTED THE WI-FI CLIENT ASSOCIATED WITH THE SETTOP TO 2.4GHz SSID");
	    } else {
		LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    // Validating IPv4 , IPV6 Address and connectivity check with IPv4 and IPv6 using Curl
	    verifyIpstatusAndConnectivityInConnectedClient(device, wifiClientDevice, testCaseId,
		    new String[] { "2", "3", "4", "5" }, "WIFI CONNECTED CLIENT 1");

	    /**
	     * STEP 6: Verify connecting the Another WiFi Client in the Setup to the 5GHz SSID
	     */
	    testStepNumber = "S6";
	    status = false;

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 6 : DESCRIPTION : VERIFY CONNECTING THE ANOTHER WI-FI CLIENT IN THE SETTOP TO 5GHz SSID");
	    LOGGER.info("STEP 6 : ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
	    LOGGER.info("STEP 6 : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to connect the associated connected client to 5GHz SSID";
	    try {
		wifi5GhzClientDevice = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(device,
			tapEnv, wifiClientDevice, BroadBandTestConstants.BAND_5GHZ);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = wifi5GhzClientDevice != null;
	    if (status) {
		LOGGER.info(
			"STEP 6:ACTUAL :SUCCESSFULLY CONNECTED THE WI-FI CLIENT ASSOCIATED WITH THE SETTOP TO 5GHz SSID");
	    } else {
		LOGGER.error("STEP 6:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    // Validating IPv4 , IPV6 Address and connectivity check with IPv4 and IPv6 using Curl
	    verifyIpstatusAndConnectivityInConnectedClient(device, wifi5GhzClientDevice, testCaseId,
		    new String[] { "7", "8", "9", "10" }, "WIFI CONNECTED CLIENT 2");
	    /**
	     * STEP 11: Retrieve Ethernet connected client from connected client list
	     */
	    testStepNumber = "S11";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 11 : DESCRIPTION : RETRIEVE ETHERNET CONNECTED CLIENT FROM CONNECTED CLIENTS LIST");
	    LOGGER.info("STEP 11 : ACTION : ETHERNET CLIENT SHOULD BE RETRIEVED");
	    LOGGER.info("STEP 11 : EXPECTED: ETHERNET CONNECTED CLIENT IS RETRIEVED SUCCESSFULLY");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to retrieve ethernet client ";
	    try {
		ethernetClient = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = ethernetClient != null;
	    if (status) {
		LOGGER.info("STEP 11:ACTUAL :Ethernet connected client is retrieved successfully");
	    } else {
		LOGGER.error("STEP 11:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    // Validating IPv4 , IPV6 Address and connectivity check with IPv4 and IPv6 using Curl
	    verifyIpstatusAndConnectivityInConnectedClient(device, ethernetClient, testCaseId,
		    new String[] { "12", "13", "14", "15" }, "ETHERNET CONNECTED CLIENT");

	    /**
	     * STEP 16: Verify enabling bridge mode using WebPA.
	     */

	    testStepNumber = "S16";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 16: DESCRIPTION : VERIFY ENABLING BRIDGE MODE ON THE DEVICE.");
	    LOGGER.info(
		    "STEP 16: ACTION : SET THE VALUE OF PARAMETER 'Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode' TO 'bridge-static'.");
	    LOGGER.info("STEP 16: EXPECTED : BRIDGE MODE MUST BE ENABLED");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to set the Lan mode to 'bridge-static'.";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
		    LanModeTypes.BRIDGE_STATIC.getLanModeValue(), BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    LOGGER.info("Please Wait for 5 min to affect bridge mode changes");
	    tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP 16:ACTUAL :SUCCESSFULLY SET THE LAN MODE TO 'bridge-static'.");
	    } else {
		LOGGER.error("STEP 16:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 17:VERIFY WEBPA PROCESS IS UP AND RUNNING
	     */
	    status = false;
	    testStepNumber = "s17";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 17 : DESCRIPTION :VERIFY WEBPA PROCESS IS UP AND RUNNING ");
	    LOGGER.info(
		    "STEP 17 : ACTION :EXECUTE WEBPA COMMAND PARAM :'Device.WiFi.SSID.10001.SSID' AND CHECK WEBPA STATUS");
	    LOGGER.info("STEP 17 : EXPECTED:WEBPA PROCESS SHOULD BE UP AND RUNNING");
	    errorMessage = "Unable to verify webpa in device";
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    if (status) {
		LOGGER.info("STEP 17:ACTUAL :WEBPA PROCESS IS UP & RUNNNING");
	    } else {
		LOGGER.error("STEP 17:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 18:Verify that the Private Wi-Fi SSIDs 2.4Ghz and 5Ghz are disabled using WebPA
	     */
	    status = false;
	    testStepNumber = "s18";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 18 : DESCRIPTION :VERIFY THAT THE PRIVATE WI-FI SSIDS 2.4GHZ AND 5GHZ ARE DISABLED USING WEBPA");
	    LOGGER.info("STEP 18 : ACTION:EXECUTE WIFI RADIO 2.4GHZ AND 5GHZ WEBPA COMMANDs");
	    LOGGER.info("STEP 18 : EXPECTED:PRIVATE SSID'S 2.4GHZ AND 5GHZ SHOULD BE DISABLED SUCCESSFULLY");
	    errorMessage = "Unable to verfiy 2.4Ghz and 5GHZ radio SSID status using webpa";
	    String wifi24GhzStatus = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS);
	    String wifi5GhzStatus = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);
	    if (wifi24GhzStatus
		    .equals(BroadBandWebPaConstants.NonDefaultWiFiParametersEnum.NONDEFAULT_VALUE_RADIO_STATUS_2_4
			    .getWebPaValue())
		    && wifi5GhzStatus
			    .equals(BroadBandWebPaConstants.NonDefaultWiFiParametersEnum.NONDEFAULT_VALUE_RADIO_STATUS_5
				    .getWebPaValue())) {
		status = true;
		LOGGER.info("STEP 18:ACTUAL:Private wifi SSID's 2.4Ghz and 5Ghz are disabled successfully");
	    } else {
		LOGGER.error("STEP 18:ACTUAL:" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);
	    /**
	     * STEP 19: Verify the 2.4GHz SSID is not broadcasted in Wifi Connected client 1.
	     */
	    testStepNumber = "s19";
	    status = false;
	    errorMessage = "Unable to retrieve the SSID from the device.";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "STEP 19 : DESCRIPTION : VERIFY 2.4GHZ SSID NAME IS NOT BROADCASTED IN WIFI CONNECTED CLIENT 1");
	    LOGGER.info("STEP 19 : ACTION : EXECUTE COMMAND 'netsh wlan show networks' ON THE CONNECTED CLIENT.");
	    LOGGER.info("STEP 19 : EXPECTED : 2.4GHz SSID NAME MUST NOT BE BROADCASTED");
	    LOGGER.info("#######################################################################################");
	    String ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    if (CommonMethods.isNotNull(ssid)) {
		errorMessage = "Wifi Connected client 1 broadcasts 2.4Ghz wifi SSID -Validation failure";
		visiblityStatus = !BroadBandConnectedClientUtils
			.scanAndVerifyVisibleSsidFromWiFiConnectedClientDevice(wifiClientDevice, ssid, tapEnv);
	    }
	    if (visiblityStatus) {
		LOGGER.info(
			"STEP 19:ACTUAL :SUCCESSFULLY VERIFIED 2.4GHz SSID IS NOT BROADCASTED - Validation Success");
	    } else {
		LOGGER.error("STEP 19:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, visiblityStatus, errorMessage, true);
	    /**
	     * STEP 20: Verify wifi Connected client 1 has IPv4 address not in DHCP range.
	     * 
	     */
	    testStepNumber = "s20";
	    status = false;
	    errorMessage = "Wifi Connected client 1 has IPv4 address in DHCP range";
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 20:DESCRIPTION:VERIFY WIFI CONNECTED CLIENT 1 IPV4 ADDRESS NOT IN DHCP RANGE");
	    LOGGER.info("STEP 20:ACTION : EXECUTE ipconfig/ifconfig AND VALIDATE IPV4 ADDRESS WITH DEVICE DHCP RANGE");
	    LOGGER.info("STEP 20:EXPECTED:INTERFACE IPV4 ADDRESS SHOULD NOT BE SHOWN");
	    LOGGER.info("#####################################################################################");
	    status = !BroadBandConnectedClientUtils.isConnClientIpv4AddrBtwnDhcpRange(tapEnv, device, wifiClientDevice);
	    if (status) {
		LOGGER.info(
			"STEP 20:ACTUAL :SUCCESSFULLY VALIDATED WIFI CONNECTED CLIENT 1 DOESN'T HAVE IPV4 ADDRESS IN DHCP RANGE");
	    } else {
		LOGGER.error("STEP 20:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 21:Verify IPv6 Address not available in Wifi connected client 1
	     * 
	     */
	    testStepNumber = "s21";
	    status = false;
	    errorMessage = "Wifi Connected 1 have IPv6 address";
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 21:DESCRIPTION: VERIFY IPV6 ADDRESS NOT AVAILABLE IN WIFI CONNECTED CLIENT 1 ");
	    LOGGER.info("STEP 21:ACTION :EXECUTE ipconfig/ifconfig AND VALIDATE IPV6 ADDRESS WITH DEVICE DHCP RANGE");
	    LOGGER.info("STEP 21:EXPECTED:INTERFACE IPV6 ADDRESS SHOULD NOT BE SHOWN");
	    LOGGER.info("#####################################################################################");
	    String osType = ((Device) wifiClientDevice).getOsType();
	    status = !BroadBandConnectedClientUtils
		    .verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType, wifiClientDevice, tapEnv);
	    if (status) {
		LOGGER.info("STEP 21: ACTUAL : Interface didnt get IPv6 Address - Validation success");
	    } else {
		LOGGER.error("STEP 21: ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 22:Verify No Internet connectivity of Wifi connected client 1 using IPV4 with curl request.
	     * 
	     */
	    testStepNumber = "s22";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 22: DESCRIPTION :VERIFY NO INTERNET CONNECTIVITY OF WIFI CONNECTED CLIENT 1 USING IPV4 WITH CURL REQUEST ");
	    LOGGER.info(
		    "STEP 22: ACTION : EXECUTE curl --connect-timeout 20 --head -4 google.com SHOULD NOT BE SUCCESSFUL");
	    LOGGER.info("STEP 22: EXPECTED: CONNECTIVITY CHECK IS FAILURE");
	    LOGGER.info("#####################################################################################");
	    BroadBandResultObject bandResultObject = BroadBandConnectedClientUtils
		    .verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv, wifiClientDevice,
			    BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION4);
	    status = !bandResultObject.isStatus();
	    errorMessage = bandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 22: ACTUAL:Validation Success Internet Connectivity unsuccessful using ipv4 with Curl request ");
	    } else {
		LOGGER.error("STEP 22: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 23:Verify No Internet connectivity of Wifi connected client 1 using IPV6 with curl request.
	     * 
	     */
	    status = false;
	    testStepNumber = "s23";
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 23: DESCRIPTION :VERIFY NO INTERNET CONNECTIVITY OF WIFI CONNECTED CLIENT 1 USING IPV6 WITH CURL REQUEST.");
	    LOGGER.info(
		    "STEP 23: ACTION : EXECUTE curl --connect-timeout 20 --head -6 google.com SHOULD NOT BE SUCCESSFUL");
	    LOGGER.info("STEP 23: EXPECTED: CONNECTIVITY CHECK IS FAILURE ");
	    LOGGER.info("#####################################################################################");
	    bandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
		    tapEnv, wifiClientDevice, BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION6);
	    status = !bandResultObject.isStatus();
	    errorMessage = bandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 23: ACTUAL:Validation Success Internet Connectivity unsuccessful using ipv6 interface");
	    } else {
		LOGGER.error("STEP 23: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 24: Verify the 5GHz SSID is not broadcasted in Wifi Connected client 2.
	     */
	    testStepNumber = "S24";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 24 : DESCRIPTION : VERIFY 5GHz SSID NAME IS NOT BROADCASTED in Wifi Connected client 2");
	    LOGGER.info("STEP 24 : ACTION : EXECUTE COMMAND 'netsh wlan show networks' ON THE CONNECTED CLIENT.");
	    LOGGER.info("STEP 24 : EXPECTED : 5GHz SSID NAME IS BROADCASTED");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to retrieve the 5GHz SSID from the device.";
	    ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    if (CommonMethods.isNotNull(ssid)) {
		errorMessage = "Wifi Connected client 2 broadcasts 5Ghz wifi SSID -Validation failure";
		visiblityStatus = !BroadBandConnectedClientUtils
			.scanAndVerifyVisibleSsidFromWiFiConnectedClientDevice(wifi5GhzClientDevice, ssid, tapEnv);
	    }
	    if (visiblityStatus) {
		LOGGER.info("STEP 24:ACTUAL :SUCCESSFULLY VERIFIED 5GHz SSID IS NOT BROADCASTED.");
	    } else {
		LOGGER.error("STEP 24:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, visiblityStatus, errorMessage, true);

	    /**
	     * STEP 25: Verify wifi Connected client 2 has IPv4 address not in DHCP range.
	     * 
	     */
	    testStepNumber = "s25";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 25:DESCRIPTION:VERIFY WIFI CONNECTED CLIENT 2 HAS IPV4 ADDRESS NOT IN DHCP RANGE.");
	    LOGGER.info("STEP 25:ACTION : CONNECTED CLIENT SHOULD NOT GET THE IPV4 ");
	    LOGGER.info("STEP 25:EXPECTED:INTERFACE IPV4 ADDRESS SHOULD NOT BE SHOWN");
	    LOGGER.info("#####################################################################################");
	    errorMessage = "Interface got the correct IPV4 address";
	    status = !BroadBandConnectedClientUtils.isConnClientIpv4AddrBtwnDhcpRange(tapEnv, device,
		    wifi5GhzClientDevice);
	    if (status) {
		LOGGER.info("STEP 25:ACTUAL :Interface didnt get the IPv4 address - Validation is success");
	    } else {
		LOGGER.error("STEP 25:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 26:Verify wifi Connected client 2 has IPv6 address .
	     * 
	     */
	    testStepNumber = "s26";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 26:DESCRIPTION: VERIFY WIFI CONNECTED CLIENT 2 HAS IPV6 ADDRESS");
	    LOGGER.info("STEP 26:ACTION : CONNECTED CLIENT SHOULD GET NOT THE IPV6 ADDRESS");
	    LOGGER.info("STEP 26:EXPECTED: IPV6 ADDRESS SHOULD NOT BE AVAILBLE");
	    LOGGER.info("#####################################################################################");
	    errorMessage = "Interface got the correct IPV6 address";
	    osType = ((Device) wifi5GhzClientDevice).getOsType();
	    status = !BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
		    osType, wifi5GhzClientDevice, tapEnv);
	    if (status) {
		LOGGER.info("STEP 26: ACTUAL : Interface didnt get the correct IPv6  address- Validation success");
	    } else {
		LOGGER.error("STEP 26: ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 27:Verify NO Internet connectivity using IPV4 with Curl request
	     * 
	     */
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 27: DESCRIPTION :VERIFY NO INTERNET CONNECTIVITY USING IPV4 WITH CURL REQUEST");
	    LOGGER.info(
		    "STEP 27: ACTION : EXECUTE curl --connect-timeout 20 --head -4 google.com SHOULD NOT BE SUCCESSFUL");
	    LOGGER.info("STEP 27: EXPECTED: CONNECTIVITY CHECK SHOULD BE FAILURE");
	    LOGGER.info("#####################################################################################");
	    testStepNumber = "s27";
	    status = false;
	    bandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
		    tapEnv, wifi5GhzClientDevice, BroadBandTestConstants.URL_GOOGLE,
		    BroadBandTestConstants.IP_VERSION4);
	    status = !bandResultObject.isStatus();
	    errorMessage = bandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 27: ACTUAL: Validation success NO Internet connectivity using ipv4 interface");
	    } else {
		LOGGER.error("STEP 27: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 28:Verify NO Internet connectivity using IPV6 with Curl request
	     * 
	     */
	    status = false;
	    testStepNumber = "s28";
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 28: DESCRIPTION :VERIFY NO INTERNET CONNECTIVITY USING IPV4 WITH CURL REQUEST");
	    LOGGER.info(
		    "STEP 28: ACTION : EXECUTE curl --connect-timeout 20 --head -6 google.com SHOULD BE NOT SUCCESSFUL");
	    LOGGER.info("STEP 28: EXPECTED: CONNECTIVITY CHECK SHOULD BE FAILURE");
	    LOGGER.info("#####################################################################################");
	    bandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
		    tapEnv, wifi5GhzClientDevice, BroadBandTestConstants.URL_GOOGLE,
		    BroadBandTestConstants.IP_VERSION6);
	    status = !bandResultObject.isStatus();
	    errorMessage = bandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 28: ACTUAL: Validation success NO Internet connectivity using ipv6 interface");
	    } else {
		LOGGER.error("STEP 28: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
	    LOGGER.info("#####################################################################################");

	    /**
	     * STEP 29: Verify whether Ethernet connected client has IPv4 address in Wan IP Broadcast.
	     * 
	     */
	    testStepNumber = "s29";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 29:DESCRIPTION:VERIFY WHETHER ETHERNET CONNECTED CLIENT HAS IPV4 ADDRESS IN WAN IP BROADCAST");
	    LOGGER.info("STEP 29:ACTION : CONNECTED CLIENT SHOULD GET THE IPV4 INTERFACE");
	    LOGGER.info("STEP 29:EXPECTED:INTERFACE IPV4 ADDRESS SHOULD  BE SHOWN");
	    LOGGER.info("#####################################################################################");
	    errorMessage = "Interface  didnt get the correct IPV4 address";
	    bandResultObject = BroadBandConnectedClientUtils.verifyLanClientWanIpAddressDeviceInBridgeMode(tapEnv,
		    device, ethernetClient);
	    if (bandResultObject.isStatus()) {
		status = true;
		LOGGER.info(
			"STEP 29:ACTUAL :Interface get the correct IPv4 address and is same as in wanip broadcast range");
	    } else {
		LOGGER.error("STEP 29:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 30:Verify whether Ethernet connected client has IPv6 address.
	     * 
	     */
	    testStepNumber = "s30";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 30:DESCRIPTION: VERIFY WHETHER ETHERNET CONNECTED CLIENT HAS IPV6  ADDRESS.");
	    LOGGER.info("STEP 30:ACTION : CONNECTED CLIENT SHOULD GET THE IPV6 ADDRESS");
	    LOGGER.info("STEP 30:EXPECTED: INTERFACE IPV6 ADDRESS SHOULD BE SHOWN");
	    LOGGER.info("#####################################################################################");
	    errorMessage = "Interface  didnt get the correct IPV6 address";
	    osType = ((Device) ethernetClient).getOsType();
	    if (CommonMethods.isNotNull(osType)) {
		status = BroadBandConnectedClientUtils
			.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType, ethernetClient, tapEnv);
	    }
	    if (status) {
		LOGGER.info("STEP 30: ACTUAL : Interface  got the correct IPv6  address");
	    } else {
		LOGGER.error("STEP 30: ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 31:Verify connectivity of Ethernet connected client using IPV4 with curl request.
	     * 
	     */
	    testStepNumber = "s31";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 31: DESCRIPTION :VERIFY CONNECTIVITY OF ETHERNET CONNECTED CLIENT USING IPV4 WITH CURL REQUEST. ");
	    LOGGER.info(
		    "STEP 31: ACTION : EXECUTE curl --connect-timeout 20 --head -4 google.com SHOULD BE SUCCESSFUL");
	    LOGGER.info("STEP 31: EXPECTED: CONNECTIVITY CHECK SHOULD RETURN STATUS AS 200");
	    LOGGER.info("#####################################################################################");
	    bandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
		    tapEnv, ethernetClient, BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION4);
	    status = bandResultObject.isStatus();
	    errorMessage = bandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 31: ACTUAL: connectivity successful using ipv4 interface");
	    } else {
		LOGGER.error("STEP 31: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 32:Verify connectivity of Ethernet connected client using IPV6 with curl request
	     * 
	     */
	    status = false;
	    testStepNumber = "s32";
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 32: DESCRIPTION :VERIFY CONNECTIVITY OF ETHERNET CONNECTED CLIENT USING IPV6 WITH CURL REQUEST");
	    LOGGER.info(
		    "STEP 32: ACTION : EXECUTE curl --connect-timeout 20 --head -6 google.com SHOULD BE SUCCESSFUL");
	    LOGGER.info("STEP 32: EXPECTED: CONNECTIVITY CHECK SHOULD RETURN STATUS AS 200");
	    LOGGER.info("#####################################################################################");
	    bandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
		    tapEnv, ethernetClient, BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.IP_VERSION6);
	    status = bandResultObject.isStatus();
	    errorMessage = bandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 32: ACTUAL: connectivity successful using ipv6 interface");
	    } else {
		LOGGER.error("STEP 32: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
	    LOGGER.info("#####################################################################################");

	    /**
	     * STEP 33: Verify disabling bridge mode using WebPA.
	     */

	    testStepNumber = "s33";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 33 : DESCRIPTION : VERIFY DISABLING BRIDGE MODE ON THE DEVICE.");
	    LOGGER.info(
		    "STEP 33 : ACTION : SET THE VALUE OF PARAMETER 'Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode' TO 'router'.");
	    LOGGER.info("STEP 33 : EXPECTED : BRIDGE MODE MUST BE ENABLED");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to set the Lan mode to 'router'.";
	    disabledBridgeMode = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
		    LanModeTypes.ROUTER.getLanModeValue());
	    LOGGER.info("Please Wait for 5 min to affect router mode changes");
	    tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
	    if (disabledBridgeMode) {
		LOGGER.info("STEP 33:ACTUAL :SUCCESSFULLY SET THE LAN MODE TO 'router'.");
	    } else {
		LOGGER.error("STEP 33:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, disabledBridgeMode, errorMessage, true);

	    /**
	     * STEP 34: Verify connecting the WiFi Client in the Setup to the 2.4GHz SSID
	     */
	    testStepNumber = "S34";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "STEP 34 : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID WITH BRIDGE MODE DISABLED");
	    LOGGER.info("STEP 34 : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
	    LOGGER.info("STEP 34 : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to connect the associated connected client to 2.4GHz SSID";
	    bandResultObject = null;
	    try {
		bandResultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
			wifiClientDevice, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = bandResultObject.isStatus();
	    if (status) {
		wifi24Status = true;
		LOGGER.info(
			"STEP 34:ACTUAL :SUCCESSFULLY CONNECTED THE WI-FI CLIENT ASSOCIATED WITH THE SETTOP TO 2.4GHz SSID WITH BRIDGE MODE DISABLED.");
	    } else {
		LOGGER.error("STEP 34:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    // Validating IPv4 , IPV6 Address and connectivity check with IPv4 and IPv6 using Curl
	    verifyIpstatusAndConnectivityInConnectedClient(device, wifiClientDevice, testCaseId,
		    new String[] { "35", "36", "37", "38" }, "WIFI CONNECTED CLIENT 1");
	    /**
	     * STEP 39: Verify connecting the WiFi Client in the Setup to the 5GHz SSID
	     */
	    testStepNumber = "S39";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 39 : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 5GHz SSID");
	    LOGGER.info("STEP 39 : ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
	    LOGGER.info("STEP 39 : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to connect the associated connected client to 5GHz SSID";
	    try {
		bandResultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
			wifi5GhzClientDevice, WiFiFrequencyBand.WIFI_BAND_5_GHZ);

	    } catch (TestException exception) {
		// Log & Suppress the exception
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = bandResultObject.isStatus();
	    if (status) {
		wifi5Status = true;
		LOGGER.info(
			"STEP 39:ACTUAL :SUCCESSFULLY CONNECTED THE WI-FI CLIENT ASSOCIATED WITH THE SETTOP TO 5GHz SSID");
	    } else {
		LOGGER.error("STEP 39:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    // Validating IPv4 , IPV6 Address and connectivity check with IPv4 and IPv6 using Curl
	    verifyIpstatusAndConnectivityInConnectedClient(device, wifi5GhzClientDevice, testCaseId,
		    new String[] { "40", "41", "42", "43" }, "WIFI CONNECTED CLIENT 2");

	    // Validating IPv4 , IPV6 Address and connectivity check with IPv4 and IPv6 using Curl
	    verifyIpstatusAndConnectivityInConnectedClient(device, ethernetClient, testCaseId,
		    new String[] { "44", "45", "46", "47" }, "ETHERNET CONNECTED CLIENT");

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING 2.4GHz WIFI RADIO WITH LAN MODE AS 'bridge-static': "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
		    errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY DISABLING THE BRIDGE MODE.");
	    LOGGER.info(
		    "POST-CONDITION 1 : ACTION : SET THE VALUE OF PARAMETER 'Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode' TO 'router'.");
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : LAN MODE MUST BE SET TO 'router'");
	    LOGGER.info("#######################################################################################");
	    try {
		if (!disabledBridgeMode) {
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
			    LanModeTypes.ROUTER.getLanModeValue(), BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

		}
		if (status) {
		    LOGGER.info("POST-CONDITION 1 : ACTUAL : LAN MODE SET TO 'router': " + status);
		} else {
		    LOGGER.error("POST_CONDITION 1 : ACTUAL: FAILURE IN SETTING MODE TO 'router' USING WEBPA");
		}
	    } catch (Exception exception2) {
		// Log & Suppress the exception
		LOGGER.error("EXCEPTION OCCURRED WHILE PERFORMING POST-CONFIGURATION 1: " + exception2.getMessage());
	    }
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 2 : DESCRIPTION : DISCONNECT WIFI RADIO 2.4GHZ SSID FROM THE DEVICE");
	    LOGGER.info("POST-CONDITION 2 : ACTION :DISCONNECT WIFI RADIO 2.4GHZ SSID ");
	    LOGGER.info("POST-CONDITION 2 : EXPECTED : WIFI RADIO 2.4GHZ SSID SHOULD BE DISCONNECTED SUCCESSFULLY");
	    LOGGER.info("#######################################################################################");
	    try {
		if (wifi24Status) {
		    String ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device,
			    tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
		    LOGGER.info("SSIDNAME:" + ssidName);
		    wifi24Status = ConnectedNattedClientsUtils.disconnectSSID(wifiClientDevice, tapEnv, ssidName);
		}
		LOGGER.info("POST CONDITION 2:ACTUAL: WIFI SSID 2.4GHZ Disconnect status:" + wifi24Status);
	    } catch (Exception exception2) {
		LOGGER.error("EXCEPTION OCCURRED WHILE PERFORMING POST CONFIGURATION 2" + exception2.getMessage());
	    }
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 3 : DESCRIPTION : DISCONNECT WIFI RADIO 5GHZ SSID FROM THE DEVICE");
	    LOGGER.info("POST-CONDITION 3 : ACTION :DISCONNECT WIFI RADIO 5GHZ SSID ");
	    LOGGER.info("POST-CONDITION 3 : EXPECTED : WIFI RADIO 5GHZ SSID SHOULD BE DISCONNECTED SUCCESSFULLY");
	    LOGGER.info("#######################################################################################");
	    try {
		if (wifi5Status) {
		    String ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device,
			    tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
		    LOGGER.info("SSIDNAME:" + ssidName);
		    wifi5Status = ConnectedNattedClientsUtils.disconnectSSID(wifi5GhzClientDevice, tapEnv, ssidName);
		}
		LOGGER.info("POST CONDITION 2:ACTUAL: WIFI SSID 5GHZ Disconnect status:" + wifi5Status);
	    } catch (Exception exception2) {
		LOGGER.error("EXCEPTION OCCURRED WHILE PERFORMING POST CONFIGURATION 2" + exception2.getMessage());
	    }
	    LOGGER.info("########################### ENDING POST CONFIGURATION ####################################");

	}
	LOGGER.info("#######################################################################################");

    }

    public void verifyIpstatusAndConnectivityInConnectedClient(Dut device, Dut connectedClient, String testCaseId,
	    String[] testNumbers, String connectionType) {
	String testStepNumber = null;
	boolean status = false;
	String errorMessage = null;
	/**
	 * STEP 1: Verify whether the Connected client got the IPv4 address from DHCP range.
	 * 
	 */
	testStepNumber = "S" + testNumbers[0];
	status = false;
	LOGGER.info("#####################################################################################");
	LOGGER.info("STEP " + testNumbers[0] + ":DESCRIPTION:VERIFY WHETHER THE " + connectionType
		+ " GOT THE IPV4 ADDRESS FROM DHCP RANGE.");
	LOGGER.info("STEP " + testNumbers[0]
		+ ":ACTION : EXECUTE ipconfig/ifconfig AND VALIDATE IPV4 ADDRESS WITH DEVICE DHCP RANGE");
	LOGGER.info("STEP " + testNumbers[0] + ":EXPECTED: IPV4 ADDRESS SHOULD  BE VALIDATED IN DHCP RANGE");
	LOGGER.info("#####################################################################################");
	errorMessage = "Interface  didnt get the correct IPV4 address";
	status = BroadBandConnectedClientUtils.isConnClientIpv4AddrBtwnDhcpRange(tapEnv, device, connectedClient);
	if (status) {
	    LOGGER.info("STEP " + testNumbers[0] + ":ACTUAL :Connected client has IPv4 address in DHCP range");
	} else {
	    LOGGER.error("STEP " + testNumbers[0] + ":ACTUAL :" + errorMessage);
	}
	tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	/**
	 * STEP 2:Verify whether Connected client obtained the IPv6 address.
	 * 
	 */
	testStepNumber = "S" + testNumbers[1];
	status = false;
	LOGGER.info("#####################################################################################");
	LOGGER.info("STEP " + testNumbers[1] + " :DESCRIPTION: VERIFY WHETHER " + connectionType
		+ " OBTAINED THE IPV6 ADDRESS.");
	LOGGER.info("STEP " + testNumbers[1]
		+ " :ACTION : EXECUTE ipconfig/ifconfig AND VALIDATE IPV6 ADDRESS WITH DEVICE DHCP RANGE");
	LOGGER.info("STEP " + testNumbers[1] + " :EXPECTED:IPV6 ADDRESS SHOULD  BE VALIDATED");
	LOGGER.info("#####################################################################################");

	errorMessage = "Interface didnt get the correct IPV6 address";
	String osType = ((Device) connectedClient).getOsType();
	status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
		connectedClient, tapEnv);
	if (status) {
	    LOGGER.info("STEP " + testNumbers[1] + " ACTUAL :Connected client has IPv6 address validated successfully");
	} else {
	    LOGGER.error("STEP " + testNumbers[1] + " ACTUAL :" + errorMessage);
	}
	tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	/**
	 * STEP 3:Verify connectivity of connected client using IPV4 with curl request.
	 * 
	 */
	testStepNumber = "S" + testNumbers[2];
	status = false;
	LOGGER.info("#####################################################################################");
	LOGGER.info("STEP " + testNumbers[2] + ": DESCRIPTION :VERIFY CONNECTIVITY OF " + connectionType
		+ " USING IPV4 WITH CURL REQUEST ");
	LOGGER.info("STEP " + testNumbers[2]
		+ ": ACTION : EXECUTE curl --connect-timeout 20 --head -4 google.com SHOULD BE SUCCESSFUL");
	LOGGER.info("STEP " + testNumbers[2] + ": EXPECTED: CONNECTIVITY CHECK SHOULD RETURN STATUS AS 200");
	LOGGER.info("#####################################################################################");
	BroadBandResultObject broadBandResultObject = BroadBandConnectedClientUtils
		.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv, connectedClient,
			BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION4);
	status = broadBandResultObject.isStatus();
	errorMessage = broadBandResultObject.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP " + testNumbers[2]
		    + ": ACTUAL: Internet connectivity successful using ipv4 with Curl request");
	} else {
	    LOGGER.error("STEP " + testNumbers[2] + ": ACTUAL: " + errorMessage);
	}
	LOGGER.info("#####################################################################################");
	tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	/**
	 * STEP 4:Verify connectivity of connected client using IPV6 with curl request
	 * 
	 */
	status = false;
	testStepNumber = "S" + testNumbers[3];
	LOGGER.info("#####################################################################################");
	LOGGER.info("STEP " + testNumbers[3] + ": DESCRIPTION :VERIFY CONNECTIVITY OF " + connectionType
		+ " USING IPV6 WITH CURL REQUEST");
	LOGGER.info("STEP " + testNumbers[3]
		+ ": ACTION : EXECUTE curl --connect-timeout 20 --head -6 google.com SHOULD BE SUCCESSFUL");
	LOGGER.info("STEP " + testNumbers[3] + ": EXPECTED: CONNECTIVITY CHECK SHOULD RETURN STATUS AS 200");
	LOGGER.info("#####################################################################################");
	broadBandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
		tapEnv, connectedClient, BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION6);
	status = broadBandResultObject.isStatus();
	errorMessage = broadBandResultObject.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP " + testNumbers[3]
		    + ": ACTUAL: Internet Connectivity successful using ipv6 with Curl request ");
	} else {
	    LOGGER.error("STEP " + testNumbers[3] + ": ACTUAL: " + errorMessage);
	}
	LOGGER.info("#####################################################################################");
	tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

    }


}

