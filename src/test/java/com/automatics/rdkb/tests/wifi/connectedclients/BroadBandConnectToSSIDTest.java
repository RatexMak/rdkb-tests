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
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants.WINDOWS_WIRELESS_MODE_OPTIONS;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils.WifiOperatingStandard;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.enums.BroadBandWhixEnumConstants.WEBPA_AP_INDEXES;
import com.automatics.rdkb.utils.BroadBandMeshUtils;

public class BroadBandConnectToSSIDTest extends AutomaticsTestBase {

	
    /**
     * Validate notification is not seen for unauthenticated clients
     * <ol>
     * <li>Pre-Condition 1 : Validate whether the required type and No of clients</li>
     * <li>Pre-Condition 2 : Perform Factory reset on the gateway device.</li>
     * <li>Pre-Condition 3 : Reactivate the gateway</li>
     * <li>Validate if the following log files , \"LM.txt.0, WiFilog.txt.0, Harvesterlog.txt.0\" are present in device
     * within 10 minutes of uptime</li>
     * <li>Customise the private WiFi SSID and password</li>
     * <li>Try to connect a client to 2.4 GHz with incorrect password</li>
     * <li>Validate the presence of connection notification at LM.txt.0</li>
     * <li>Validate the presence of connection notification at WiFilog.txt.0</li>
     * <li>Validate the presence of connection notification at Harvesterlog.txt.0</li>
     * <li>Validate that the unauthenticated client is not present in host table via WebPa</li>
     * <li>Connect a client to 2.4 GHz private SSID using correct password</li>
     * <li>Validate the presence of connection notification within 30 seconds of connection at LM.txt.0</li>
     * <li>Validate the presence of connection notification at WiFilog.txt.0</li>
     * <li>Validate that the authenticated client is present in host table via WebPa</li>
     * <li>Try to connect a client to 5 GHz with incorrect password</li>
     * <li>Validate the presence of connection notification at LM.txt.0</li>
     * <li>Validate the presence of connection notification at WiFilog.txt.0</li>
     * <li>Validate the presence of connection notification at Harvesterlog.txt.0</li>
     * <li>Validate that the unauthenticated client is not present in host table via WebPa</li>
     * <li>Connect a client to 5 GHz private SSID using correct password</li>
     * <li>Validate the presence of connection notification within 30 seconds of connection at LM.txt.0</li>
     * <li>Validate the presence of connection notification at WiFilog.txt.0</li>
     * <li>Validate that the authenticated client is present in host table via WebPa</li>
     * </ol>
     * 
     * @param device
     * @author SATHURYA RAVI
     * @refactor Athira
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-CC-AUTH-1001")

    public void validateNotificationForUnauthenticatedClients(Dut device) {
	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String ssid = null;
	String password = null;
	String response = null;
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-WIFI-CC-AUTH-001";

	LOGGER.info("###################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CC-AUTH-1001");
	LOGGER.info("TEST DESCRIPTION: Validate notification is not seen for unauthenticated clients");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("Pre-Condition 1 : Validate whether the required type and No of clients");
	LOGGER.info("Pre-Condition 2 : Perform Factory reset on the gateway device.");
	LOGGER.info("Pre-Condition 3 : Reactivate the gateway");
	LOGGER.info(
		"1. Validate if the following log files , \"LM.txt.0, WiFilog.txt.0, Harvesterlog.txt.0\" are present in device within 10 minutes of uptime ");
	LOGGER.info("2. Customise the private WiFi SSID and password ");
	LOGGER.info("3. Try to connect a client to 2.4 GHz with incorrect password ");
	LOGGER.info("4. Validate the presence of connection notification at LM.txt.0 ");
	LOGGER.info("5. Validate the presence of connection notification at WiFilog.txt.0 ");
	LOGGER.info("6. Validate the presence of connection notification at Harvesterlog.txt.0 ");
	LOGGER.info("7. Validate that the unauthenticated client is not present in host table via WebPa ");
	LOGGER.info("8. Connect a client to 2.4 GHz private SSID using correct password  ");
	LOGGER.info(
		"9. Validate the presence of connection notification within 30 seconds of connection at LM.txt.0  ");
	LOGGER.info("10. Validate the presence of connection notification at WiFilog.txt.0  ");
	LOGGER.info("11. Validate that the authenticated client is present in host table via WebPa  ");
	LOGGER.info("12. Try to connect a client to 5 GHz with incorrect password  ");
	LOGGER.info("13. Validate the presence of connection notification at LM.txt.0 ");
	LOGGER.info("14. Validate the presence of connection notification at WiFilog.txt.0 ");
	LOGGER.info("15. Validate the presence of connection notification at Harvesterlog.txt.0  ");
	LOGGER.info("16. Validate that the unauthenticated client is not present in host table via WebPa  ");
	LOGGER.info("17. Connect a client to 5 GHz private SSID using correct password  ");
	LOGGER.info(
		"18. Validate the presence of connection notification within 30 seconds of connection at LM.txt.0  ");
	LOGGER.info("19. Validate the presence of connection notification at WiFilog.txt.0  ");
	LOGGER.info("20. Validate that the authenticated client is present in host table via WebPa  ");
	LOGGER.info("###################################################################################");
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION 1 :DESCRIPTION : Validate whether the required type and No of clients"
		    + " are available to proceed ");
	    LOGGER.info("PRE-CONDITION 1 :ACTION : Get the no of client devices and validate the type ");
	    LOGGER.info("PRE-CONDITION 1 : EXPECTED : Atleast two windows clients should be available ");

	    final List<Dut> dualBandDevices = BroadBandConnectedClientUtils.executePreconditionForDeviceList(device,
		    tapEnv);

	    status = null != dualBandDevices && dualBandDevices.size() == BroadBandTestConstants.CONSTANT_2;
	    if (status)  {
		    LOGGER.info("PRE-CONDITION 1 ACTUAL : Validated required number of windows clients are available.");
		} 
	    if (!status) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED : Unable to get the required no of clients "
			+ device.getHostMacAddress());
	    }

	    LOGGER.info("PRE-CONDITION2 :DESCRIPTION : perform factory reset on the device ");
	    LOGGER.info("PRE-CONDITION2 :ACTION : perform factory reset via WebPa / SNMP ");
	    LOGGER.info("PRE-CONDITION2 : EXPECTED : the device should be get factory reset ");

	    /**
	     * PRE-CONDITION : Factory Reset and reactivate the gateway.
	     */
	    BroadBandPreConditionUtils.executePreConditionToFactoryResetAndReacitivateDevice(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_2, true);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    stepNum = "S1";
	    errorMessage = "The required files LM.txt.0, WiFilog.txt.0,Harvesterlog.txt.0 are not present on the device";
	    status = false;
	    long startTime = System.currentTimeMillis();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Validate if the following log files , \"LM.txt.0, WiFilog.txt.0,"
		    + " Harvesterlog.txt.0\" are present in device within 10 minutes of uptime ");
	    LOGGER.info("STEP 1: ACTION : a) execute the following command on the device, \"ls -lrt /rdklogs/"
		    + "logs/Harvesterlog.txt.0 , ls -lrt /rdklogs/logs/WiFilog.txt.0 , ls -lrt /rdklogs/logs/LM.txt.0\"");
	    LOGGER.info("STEP 1: EXPECTED : The files should be present on the device");
	    LOGGER.info("**********************************************************************************");
	    do {
		LOGGER.info("Waiting for 30 seconds...");
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		status = CommonMethods.isAtomSyncAvailable(device, tapEnv)
			? BroadBandCommonUtils.isFilePresentOnDeviceAtom(tapEnv, device,
				BroadBandTestConstants.FILE_NAME_HARVESTER)
				&& BroadBandCommonUtils.isFilePresentOnDeviceAtom(tapEnv, device,
					BroadBandTestConstants.FILE_NAME_WIFILOG)
			: BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
				BroadBandTestConstants.FILE_NAME_HARVESTER)
				&& BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
					BroadBandTestConstants.FILE_NAME_WIFILOG)
				&& BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
					BroadBandTestConstants.FILE_NAME_LM);

	    } while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : The files are present on the device");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "Attempt to customize the private SSID and password has failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Customise the private WiFi SSID and password");
	    LOGGER.info("STEP 2: ACTION : Execute WebPa SET command on the object Device.WiFi.SSID.{i}.SSID,"
		    + "Device.WiFi.SSID.{i}.SSID, "
		    + "Device.WiFi.AccessPoint.{i}.Security.X_COMCAST-COM_KeyPassphrase, "
		    + ".Security.X_COMCAST-COM_KeyPassphrase  ");
	    LOGGER.info("STEP 2: EXPECTED : The SSID and passphrase should get customized successfully");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP  2: ACTUAL : Attempt to customise the private " + "SSID and password is successful");
		LOGGER.info("Waiting for 90 seconds for the changes to take effect");
		tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
	    } else {
		LOGGER.error("STEP  2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = "The gateway is accepting unauthenticated clients ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Try to connect a client to 2.4 GHz with incorrect password ");
	    LOGGER.info("STEP 3: ACTION : Connect to 2.4 and 5 GHz wifi using below commands Linux :nmcli "
		    + "dev wifi connect <ssid> password <passwd>");
	    LOGGER.info("STEP 3: EXPECTED : The client should not get connected to the gateway");
	    LOGGER.info("**********************************************************************************");
	    ssid = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID);
	    password = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_2GHZ_PASSPHRASE);
	    LOGGER.info(
		    "SSID name and phrase is not obtained for the gateway device. Failed to connect to Broadband Gateway device. SSID NAME - "
			    + ssid + " SSID Passphrase -" + password);
	    if (CommonMethods.isNotNull(ssid) && CommonMethods.isNotNull(password)) {
		password = password + BroadBandTestConstants.SINGLE_HASH_TERMINATING_CHAR;
		status = !ConnectedNattedClientsUtils
			.connectToSSID(dualBandDevices.get(BroadBandTestConstants.CONSTANT_0), tapEnv, ssid, password);
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : The gateway is not accepting unauthenticated clients");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S4";
	    errorMessage = "The notification for unathenticated clients are seen in LM.txt.0 ";
	    status = false;
	    String clientMac = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Validate the presence of connection notification at LM.txt.0 ");
	    LOGGER.info("STEP 4: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat LM.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 4: EXPECTED : The log file should not have any notification");
	    LOGGER.info("**********************************************************************************");
	    clientMac = BroadBandConnectedClientUtils.getIpOrMacFromWindowsConnectedClient(
		    dualBandDevices.get(BroadBandTestConstants.CONSTANT_0), tapEnv, false);
	    status = !BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_LM, false);
	    if (status) {
		LOGGER.info(
			"STEP  4: ACTUAL : The notification for unathenticated " + "clients are not seen in LM.txt.0 ");
	    } else {
		LOGGER.error("STEP  4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S5";
	    errorMessage = "The notification for unathenticated clients are seen in WiFilog.txt.0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Validate the presence of connection notification at WiFilog.txt.0 ");
	    LOGGER.info("STEP 5: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat WiFilog.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 5: EXPECTED : The log file should not have any notification");
	    LOGGER.info("**********************************************************************************");
	    status = !BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_WIFILOG, false);
	    if (status) {
		LOGGER.info("STEP  5: ACTUAL : The notification for unathenticated "
			+ "clients are not seen in WiFilog.txt.0 ");
	    } else {
		LOGGER.error("STEP  5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S6";
	    errorMessage = "The notification for unathenticated clients are seen in Harvesterlog.txt.0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Validate the presence of connection notification at Harvesterlog.txt.0 ");
	    LOGGER.info("STEP 6: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat Harvesterlog.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 6: EXPECTED : The log file should not have any notification");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_HARVESTER, false);
	    if (status) {
		LOGGER.info("STEP  6: ACTUAL : The notification for unathenticated "
			+ "clients are not seen in Harvesterlog.txt.0 ");
	    } else {
		LOGGER.error("STEP  6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S7";
	    errorMessage = "An entry for unauthenticated client is found in host table  ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Validate that the unauthenticated client is not "
		    + "present in host table via WebPa ");
	    LOGGER.info("STEP 7: ACTION :  Execute WebPa GET command on the object Device.WiFi."
		    + "X_RDKCENTRAL-COM_GoodRssiThreshold ");
	    LOGGER.info("STEP 7: EXPECTED : The host table entry should not be present");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isNotNull(clientMac)) {
		status = !BroadBandConnectedClientUtils.validateIfClientsAreAddedToHostTable(device, tapEnv,
			new ArrayList<Dut>() {
			    {
				add(dualBandDevices.get(BroadBandTestConstants.CONSTANT_0));
			    }
			});
	    }
	    if (status) {
		LOGGER.info("STEP  7: ACTUAL : An entry for unauthenticated client is not found in host table ");
	    } else {
		LOGGER.error("STEP  7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S8";
	    errorMessage = "The gateway is not accepting authenticated clients ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : connect a client to 2.4 GHz with incorrect password ");
	    LOGGER.info("STEP 8: ACTION : Connect to 2.4 and 5 GHz wifi using below commands Linux :nmcli "
		    + "dev wifi connect <ssid> password <passwd>");
	    LOGGER.info("STEP 8: EXPECTED : The client should not get connected to the gateway");
	    LOGGER.info("**********************************************************************************");
	    ssid = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID);
	    password = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_2GHZ_PASSPHRASE);
	    LOGGER.info(
		    "SSID name and phrase is not obtained for the gateway device. Failed to connect to Broadband Gateway device. SSID NAME - "
			    + ssid + " SSID Passphrase -" + password);
	    if (CommonMethods.isNotNull(ssid) && CommonMethods.isNotNull(password)) {
		status = ConnectedNattedClientsUtils
			.connectToSSID(dualBandDevices.get(BroadBandTestConstants.CONSTANT_0), tapEnv, ssid, password);
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : The gateway is accepting authenticated clients");
		LOGGER.info("Waiting for two minute....");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S9";
	    errorMessage = "The notification for authenticated clients are not seen in LM.txt.0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Validate the presence of connection notification at LM.txt.0 ");
	    LOGGER.info("STEP 9: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat LM.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 9: EXPECTED : The log file should have connection notification");
	    LOGGER.info("**********************************************************************************");
	    clientMac = BroadBandConnectedClientUtils.getIpOrMacFromWindowsConnectedClient(
		    dualBandDevices.get(BroadBandTestConstants.CONSTANT_0), tapEnv, false);
	    status = BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_WIFILOG, true);
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : The notification for authenticated " + "clients are not seen in LM.txt.0 ");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S10";
	    errorMessage = "The notification for authenticated clients are not seen in WiFilog.txt.0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Validate the presence of connection notification at WiFilog.txt.0 ");
	    LOGGER.info("STEP 10: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat WiFilog.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 10: EXPECTED : The log file should have a notification");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_WIFILOG, true);
	    if (status) {
		LOGGER.info("STEP  10: ACTUAL : The notification for authenticated "
			+ "clients are seen in WiFilog.txt.0 ");
	    } else {
		LOGGER.error("STEP  10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S11";
	    errorMessage = "An entry for unauthenticated client is not found in host table  ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Validate that the authenticated client is "
		    + "present in host table via WebPa ");
	    LOGGER.info("STEP 11: ACTION :  Execute WebPa GET command on the object Device.WiFi."
		    + "X_RDKCENTRAL-COM_GoodRssiThreshold using the command");
	    LOGGER.info("STEP 11: EXPECTED : The host table entry should be present");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isNotNull(clientMac)) {
		status = BroadBandConnectedClientUtils.validateIfClientsAreAddedToHostTable(device, tapEnv,
			new ArrayList<Dut>() {
			    {
				add(dualBandDevices.get(BroadBandTestConstants.CONSTANT_0));
			    }
			});
	    }
	    if (status) {
		LOGGER.info("STEP  11: ACTUAL : An entry for authenticated client is found in host table ");
	    } else {
		LOGGER.error("STEP  11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S12";
	    errorMessage = "The gateway is accepting unauthenticated clients ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Try to connect a client to 5 GHz with incorrect password ");
	    LOGGER.info("STEP 12: ACTION : Connect to 5 GHz wifi using below commands Linux :nmcli "
		    + "dev wifi connect <ssid> password <passwd>");
	    LOGGER.info("STEP 12: EXPECTED : The client should not get connected to the gateway");
	    LOGGER.info("**********************************************************************************");
	    ssid = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_SSID_10101_SSID);
	    password = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_5GHZ_PASSPHRASE);
	    LOGGER.info(
		    "SSID name and phrase is not obtained for the gateway device. Failed to connect to Broadband Gateway device. SSID NAME - "
			    + ssid + " SSID Passphrase -" + password);
	    if (CommonMethods.isNotNull(ssid) && CommonMethods.isNotNull(password)) {
		password = password + BroadBandTestConstants.SINGLE_HASH_TERMINATING_CHAR;
		status = !ConnectedNattedClientsUtils
			.connectToSSID(dualBandDevices.get(BroadBandTestConstants.CONSTANT_1), tapEnv, ssid, password);
	    }
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : The gateway is not accepting unauthenticated clients");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S13";
	    errorMessage = "The notification for unathenticated clients are seen in LM.txt.0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Validate the presence of connection notification at LM.txt.0 ");
	    LOGGER.info("STEP 13: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat LM.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 13: EXPECTED : The log file should not have any notification");
	    LOGGER.info("**********************************************************************************");
	    clientMac = BroadBandConnectedClientUtils.getIpOrMacFromWindowsConnectedClient(
		    dualBandDevices.get(BroadBandTestConstants.CONSTANT_1), tapEnv, false);
	    status = !BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_LM, false);
	    if (status) {
		LOGGER.info(
			"STEP 13: ACTUAL : The notification for unathenticated " + "clients are not seen in LM.txt.0 ");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S14";
	    errorMessage = "The notification for unathenticated clients are seen in WiFilog.txt.0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Validate the presence of connection notification at WiFilog.txt.0 ");
	    LOGGER.info("STEP 14: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat WiFilog.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 14: EXPECTED : The log file should not have any notification");
	    LOGGER.info("**********************************************************************************");
	    status = !BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_WIFILOG, false);
	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : The notification for unathenticated "
			+ "clients are not seen in WiFilog.txt.0 ");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S15";
	    errorMessage = "The notification for unathenticated clients are seen in Harvesterlog.txt.0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION : Validate the presence of connection notification at Harvesterlog.txt.0 ");
	    LOGGER.info("STEP 15: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat Harvesterlog.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 15: EXPECTED : The log file should not have any notification");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_HARVESTER, false);
	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : The notification for unathenticated "
			+ "clients are not seen in Harvesterlog.txt.0 ");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S16";
	    errorMessage = "An entry for unauthenticated client is found in host table  ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Validate that the unauthenticated client is not "
		    + "present in host table via WebPa ");
	    LOGGER.info("STEP 16: ACTION :  Execute WebPa GET command on the object Device.WiFi."
		    + "X_RDKCENTRAL-COM_GoodRssiThreshold using the command");
	    LOGGER.info("STEP 16: EXPECTED : The host table entry should not be present");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isNotNull(clientMac)) {
		status = !BroadBandConnectedClientUtils.validateIfClientsAreAddedToHostTable(device, tapEnv,
			new ArrayList<Dut>() {
			    {
				add(dualBandDevices.get(BroadBandTestConstants.CONSTANT_1));
			    }
			});
	    }
	    if (status) {
		LOGGER.info("STEP  16: ACTUAL : An entry for unauthenticated client is not found in host table ");
	    } else {
		LOGGER.error("STEP  16: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S17";
	    errorMessage = "The gateway is not accepting authenticated clients ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : connect a client to 2.4 GHz with incorrect password ");
	    LOGGER.info("STEP 17: ACTION : Connect to 2.4 and 5 GHz wifi using below commands Linux :nmcli "
		    + "dev wifi connect <ssid> password <passwd>");
	    LOGGER.info("STEP 17: EXPECTED : The client should not get connected to the gateway");
	    LOGGER.info("**********************************************************************************");
	    ssid = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_SSID_10101_SSID);
	    password = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_5GHZ_PASSPHRASE);
	    LOGGER.info(
		    "SSID name and phrase is not obtained for the gateway device. Failed to connect to Broadband Gateway device. SSID NAME - "
			    + ssid + " SSID Passphrase -" + password);
	    if (CommonMethods.isNotNull(ssid) && CommonMethods.isNotNull(password)) {
		status = ConnectedNattedClientsUtils
			.connectToSSID(dualBandDevices.get(BroadBandTestConstants.CONSTANT_1), tapEnv, ssid, password);
	    }
	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : The gateway is accepting authenticated clients");
		LOGGER.info("Waiting for two minute....");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S18";
	    errorMessage = "The notification for authenticated clients are not seen in LM.txt.0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION : Validate the presence of connection notification at LM.txt.0 ");
	    LOGGER.info("STEP 18: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat LM.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 18: EXPECTED : The log file should have connection notification");
	    LOGGER.info("**********************************************************************************");
	    clientMac = BroadBandConnectedClientUtils.getIpOrMacFromWindowsConnectedClient(
		    dualBandDevices.get(BroadBandTestConstants.CONSTANT_1), tapEnv, false);
	    status = BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_LM, true);
	    if (status) {
		LOGGER.info(
			"STEP 18: ACTUAL : The notification for authenticated " + "clients are not seen in LM.txt.0 ");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S19";
	    errorMessage = "The notification for authenticated clients are not seen in WiFilog.txt.0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION : Validate the presence of connection notification at WiFilog.txt.0 ");
	    LOGGER.info("STEP 19: ACTION : execute the following command inside the RG console of the"
		    + " gateway, cat WiFilog.txt.0|grep -i 10:56:11:8B:06:D1");
	    LOGGER.info("STEP 19: EXPECTED : The log file should have a notification");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.validatePresenceOfConnectNotification(device, tapEnv, clientMac,
		    BroadBandTestConstants.FILE_NAME_WIFILOG, true);
	    if (status) {
		LOGGER.info("STEP  19: ACTUAL : The notification for authenticated "
			+ "clients are seen in WiFilog.txt.0 ");
	    } else {
		LOGGER.error("STEP  19: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S20";
	    errorMessage = "An entry for authenticated client is not found in host table  ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 20: DESCRIPTION : Validate that the authenticated client is "
		    + "present in host table via WebPa ");
	    LOGGER.info("STEP 20: ACTION :  Execute WebPa GET command on the object Device.WiFi."
		    + "X_RDKCENTRAL-COM_GoodRssiThreshold using the command");
	    LOGGER.info("STEP 20: EXPECTED : The host table entry should be present");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isNotNull(clientMac)) {
		status = BroadBandConnectedClientUtils.validateIfClientsAreAddedToHostTable(device, tapEnv,
			new ArrayList<Dut>() {
			    {
				add(dualBandDevices.get(BroadBandTestConstants.CONSTANT_1));
			    }
			});
	    }
	    if (status) {
		LOGGER.info("STEP  20: ACTUAL : An entry for authenticated client is found in host table ");
	    } else {
		LOGGER.error("STEP  20: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CC-AUTH-1001");
    }
    
	/**
	 * Test to verify that 802.11a should not be able to connect to CM when
	 * operating standard is b,g,n , g,n mode
	 * 
	 * <li>Pre condition1: Get the default operating standard for 2.4 and 5 GHz
	 * radio</li>
	 * <li>Pre condition2: Get a Wifi client with intel driver supported</li>
	 * <li>1. Check and change the wireless mode on the gateway via webpa for 2.4GHz
	 * to b,g,n</li>
	 * <li>2. Change the wireless mode on the windows clients to 802.11 a</li>
	 * <li>3. Set different SSIDs to both the private radios</li>
	 * <li>4. Connect clients to 2.4 GHz SSID and verify connection status</li>
	 * <li>5. Check and change the wireless mode on the gateway via webpa for 2.4GHz
	 * to g,n</li>
	 * <li>6. Connect clients to 2.4 GHz SSID and verify connection status</li>
	 * <li>Post condition 1: Change the wireless mode back to default value</li>
	 * <li>Post condition 2: Change the wireless mode on the windows client to
	 * support both 2.4 and 5 GHz radio</li>
	 * 
	 * @author Sathurya Ravi
	 * @refactor Athira
	 * 
	 * @param device
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-CC-WIN-5003")
	public void testToValidate80211aClientsAreNotConnectingtoGatewayWithbgnMode(Dut device) {
		// Variable Declaration begins
		String testCaseId = "";
		String stepNum = "";
		String errorMessage = "";
		int stepNumber = 1;
		boolean status = false;
		List<Dut> dualBandDevices = null;
		String operating_Mode_Default_2 = null;
		Dut connectedClient = null;
		int postConStepNumber = 1;
		boolean isDefaultWirelessModeChanged = false;
		// Variable Declaration Ends
		testCaseId = "TC-RDKB-WIFI-CC-WIN-003";
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CC-WIN-5003");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify that 802.11a should not be able to connect to CM when operating standard is 'b,g,n', 'g,n' mode");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("Pre condition1: Get the default operating standard for 2.4 and 5 GHz radio ");
		LOGGER.info("Pre condition2: Get a Wifi client with intel driver supported ");
		LOGGER.info("1. Check and change the wireless mode on the gateway via webpa for 2.4GHz to b,g,n ");
		LOGGER.info("2. Change the wireless mode on the windows clients to 802.11 a ");
		LOGGER.info("3. Set different SSIDs to both the private radios ");
		LOGGER.info("4. Connect clients to 2.4 GHz SSID and verify connection status");
		LOGGER.info("5. Check and change the wireless mode on the gateway via webpa for 2.4GHz to g,n ");
		LOGGER.info("6. Connect clients to 2.4 GHz SSID and verify connection status ");
		LOGGER.info("Post condition 1: Change the wireless mode back to default value ");
		LOGGER.info(
				"Post condition 2: Change the wireless mode on the windows client to support both 2.4 and 5 GHz radio ");
		LOGGER.info("#######################################################################################");

		try {
			LOGGER.info("######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			/**
			 * PRE-CONDITION 1 : VERIFY THE DEFAULT OPERATING STANDARD FOR 2 GHz
			 */
			operating_Mode_Default_2 = BroadBandPreConditionUtils.executePreConditionToGetDefaultOperStandard(device,
					tapEnv, BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.BAND_2_4GHZ);

			/**
			 * PRE-CONDITION 2 : VERIFY THE PRIVATE WIFI 2.4 GHz AND 5 GHz SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			dualBandDevices = BroadBandPreConditionUtils
					.executePreConditionToGetBothPrivateWiFiSsidsVisibleIntelDevices(device, tapEnv,
							BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.CONSTANT_1);
			connectedClient = dualBandDevices.get(BroadBandTestConstants.CONSTANT_0);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1 : CHECK AND CHANGE THE WIRELESS MODE ON THE GATEWAY VIA WEBPA FOR
			 * 2.4GHZ
			 */
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Attempt to change operating standards for 2.4GHz radios has failed";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " :DESCRIPTION : Check and change the wireless mode on the gateway via webpa for 2.4GHz ");
			LOGGER.info("STEP " + stepNumber + " :ACTION : Execute webpa set on the parameter "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
			LOGGER.info(
					"STEP " + stepNumber + " : EXPECTED : The webpa set should be successful for operating standards ");
			LOGGER.info("**********************************************************************************");

			/* Checking for a specific device model */

			boolean deviceStatus = false;
			boolean specificPlatform = false;

			try {
				deviceStatus = Boolean.parseBoolean(BroadbandPropertyFileHandler
						.getAutomaticsPropsValueByResolvingPlatform(device, BroadBandTestConstants.DEVICE_STATUS));
			} catch (Exception e) {
				deviceStatus = false;
				LOGGER.info("No platform dpeendent extensions are mentioned in  automatics properties");

			}

			try {
				specificPlatform = Boolean.parseBoolean(BroadbandPropertyFileHandler
						.getAutomaticsPropsValueByResolvingPlatform(device, BroadBandTestConstants.SPECIFIC_PLATFORM));
			} catch (Exception e) {
				specificPlatform = false;
				LOGGER.info("No platform dpeendent extensions are mentioned in  automatics properties");

			}

			if (!deviceStatus && !DeviceModeHandler.isBusinessClassDevice(device)) {
				try {
					status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
							!specificPlatform ? WifiOperatingStandard.OPERATING_STANDARD_B_G_N.getOperatingmode()
									: WifiOperatingStandard.OPERATING_STANDARD_G_N_AX.getOperatingmode());
				} catch (Exception e) {
					errorMessage = e.getMessage();
					LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING WIRELESS MODE : " + errorMessage);
				}
				if (!status) {
					status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
							WebPaDataTypes.STRING.getValue(),
							!specificPlatform ? WifiOperatingStandard.OPERATING_STANDARD_B_G_N.getOperatingmode()
									: WifiOperatingStandard.OPERATING_STANDARD_G_N_AX.getOperatingmode());
				}
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL : Operating standards for 2.4GHz radios has been set to b,g,n successfully");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			} else {
				LOGGER.info(
						"This step is not applicable Bussiness Calss devices and a particular platform which operating standard b,g,n is not supported by them");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 2 : CHANGE THE WIRELESS MODE ON THE WINDOWS CLIENTS TO 802.11 A
			 */
			++stepNumber;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Attempt to change wireless mode to 802.11 a has failed";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Change the wireless mode on the windows clients to 802.11 a");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute the command , \"PowerShell.exe -command Set-NetAdapterAdvancedProperty "
					+ "-DisplayName /\"Wireless Mode/\" -DisplayValue \"802.11 a\"\" and cross validate the set "
					+ "result using command \"PowerShell.exe -command Get-NetAdapterAdvancedProperty -DisplayName "
					+ "\"Wireless Mode\"\"");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : The set and get operations should be successful and the wireless mode should be \"802.11 a\"");
			LOGGER.info("**********************************************************************************");
			status = BroadBandMeshUtils.changeWirelessModeOnWindowsClients(tapEnv, connectedClient,
					WINDOWS_WIRELESS_MODE_OPTIONS.A_802_11);
			if (status) {
				LOGGER.info("STEP " + stepNum + ": ACTUAL : Wireless mode has been changed to 802.11 a successfully");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 3 : SET DIFFERENT SSIDS TO BOTH THE PRIVATE RADIOS
			 */
			++stepNumber;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Attempt to set different SSID to private wifi has failed";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Set different SSIDs to both the private radios");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa set for both private radios");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Webpa set should be successful");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWiFiUtils.changeWiFiSsidAndPassphraseFor24And5Ghz(device, WEBPA_AP_INDEXES.PRIVATE_WIFI);

			if (status) {
				LOGGER.info("STEP " + stepNum + ": ACTUAL : Set different SSIDs to 2.4 and 5 GHz private wifi");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 4 : CONNECT CLIENTS TO 2.4 GHZ SSID AND VERIFY CONNECTION STATUS
			 */
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "802.11 a client is getting connected to gateway in b,g,n mode";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Connect clients to 2.4 GHz SSID and "
					+ " verify connection status ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Connect to 2.4 GHz SSID private wifi using below commands"
					+ " Linux :nmcli dev wifi connect <ssid> password <passwd>");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : The clients should not get connected");
			LOGGER.info("**********************************************************************************");
			/*
			 * Checking if current device is Business class or specific device which does
			 * not support the operating standard b,g,n
			 */

			LOGGER.info(" deviceStatus is " + deviceStatus + " DeviceModeHandler.isBusinessClassDevice(device) "
					+ DeviceModeHandler.isBusinessClassDevice(device));

			if (!deviceStatus && !DeviceModeHandler.isBusinessClassDevice(device)) {
				LOGGER.info(
						" Its non Bussiness class devices and not the specific device platform which is not supporting the operating standard b,g,n");
				status = !BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device, tapEnv, connectedClient,
						WEBPA_AP_INDEXES.PRIVATE_WIFI, WiFiFrequencyBand.WIFI_BAND_2_GHZ).isStatus();
				LOGGER.info(" status " + status);
				if (status) {
					isDefaultWirelessModeChanged = status;
					LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully validated that 802.11 a client "
							+ "does not connect to gateway radio in b,g,n mode");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			} else {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : This step is not applicable for Bussiness class devices and specific device platform which not supporting the operating standard b,g,n");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 5: CHECK AND CHANGE THE WIRELESS MODE ON THE GATEWAY VIA WEBPA FOR
			 * 2.4GHZ
			 */
			++stepNumber;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Attempt to change operating standards for 2.4GHz radios has failed";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " :DESCRIPTION : Check and change the wireless mode on the gateway via webpa for 2.4GHz ");
			LOGGER.info("STEP " + stepNumber + " :ACTION : Execute webpa set on the parameter "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
			LOGGER.info(
					"STEP " + stepNumber + " : EXPECTED : The webpa set should be successful for operating standards ");
			LOGGER.info("**********************************************************************************");
			try {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
						WifiOperatingStandard.OPERATING_STANDARD_G_N.getOperatingmode());
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING WIRELESS MODE : " + errorMessage);
			}
			if (!status) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(),
						WifiOperatingStandard.OPERATING_STANDARD_G_N.getOperatingmode());
			}
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL : Operating standards for 2.4GHz radios has been set to b,g,n successfully");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL : Operating standards for 2.4GHz radios has been set to g,n successfully");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 6 : CONNECT CLIENTS TO 2.4 GHZ SSID AND VERIFY CONNECTION STATUS
			 */
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "802.11 a client is getting connected to gateway in g,n mode";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Connect clients to 2.4 GHz SSID and "
					+ " verify connection status ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Connect to 2.4 GHz SSID private wifi using below commands"
					+ " Linux :nmcli dev wifi connect <ssid> password <passwd>");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : The clients should not get connected.");
			LOGGER.info("**********************************************************************************");
			status = !BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device, tapEnv, connectedClient,
					WEBPA_AP_INDEXES.PRIVATE_WIFI, WiFiFrequencyBand.WIFI_BAND_2_GHZ).isStatus();
			if (status) {
				isDefaultWirelessModeChanged = status;
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully validated that 802.11 a client "
						+ "does not connect to gateway radio in g,n mode");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		} catch (Exception e) {
			LOGGER.error(
					"Exception occured while trying to verify that the 802.11 b , 802.11 g clients are connecting to gateway when wireless mode is in 802.11 b,g,n for 2.4 GHz and a,n,ac for 5 GHz radio",
					e);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");

			/**
			 * POST-CONDITION 1 : SET THE DEFAULT OPERATING STANDARD FOR 2.4 GHZ
			 */
			if (operating_Mode_Default_2 != null) {
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": DESCRIPTION : Set the default value for operating standard 2.4 GHz.");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": ACTION : Set the default value for operating standard 2.4 GHz using webpa param "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": EXPECTED :The 2.4 GHz operating standard must be set to default value");
				LOGGER.info("#######################################################################################");
				errorMessage = "Unable to set 2.4 GHz operating standard default value.";
				try {
					status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
							operating_Mode_Default_2);
				} catch (TestException exception) {
					LOGGER.error(errorMessage + " : " + exception.getMessage());
				}
				if (!status) {
					status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
							WebPaDataTypes.STRING.getValue(), operating_Mode_Default_2);
				}
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : Successfully set the default value for operating standard 2.4 GHz.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}

				/**
				 * POST-CONDITION 2 : SET THE DEFAULT WIRELESS MODE IN CONNECTED CLIENT
				 */
				if (isDefaultWirelessModeChanged) {
					postConStepNumber++;
					errorMessage = null;
					status = false;
					LOGGER.info(
							"#######################################################################################");
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : DESCRIPTION : Set the default wireless mode in connected client");
					LOGGER.info("POST-CONDITION " + postConStepNumber + " : ACTION :  Execute command "
							+ BroadBandTestConstants.COMMAND_SET_WIRELESS_MODE);
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : EXPTECTED : Must set the default wireless mode in connected clinet");
					LOGGER.info(
							"#######################################################################################");
					errorMessage = "Unable to set the default wireless mode in connected client";
					try {
						status = BroadBandMeshUtils.changeWirelessModeOnWindowsClients(tapEnv, connectedClient,
								WINDOWS_WIRELESS_MODE_OPTIONS.A_B_G_802_11);
					} catch (Exception exception) {
						errorMessage += exception.getMessage();
					}
					if (status) {
						LOGGER.info("POST-CONDITION " + postConStepNumber
								+ ": ACTUAL :  Successfully set the connected client wireless mode");
					} else {
						LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL : " + errorMessage);
					}

				}
			}

		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CC-WIN-5003");
	}
}
