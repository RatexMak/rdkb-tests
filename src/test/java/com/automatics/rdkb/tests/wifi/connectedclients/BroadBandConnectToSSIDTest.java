package com.automatics.rdkb.tests.wifi.connectedclients;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

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
		    + ".Security.X_COMCAST-COM_KeyPassphrase");
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
	    LOGGER.info("STEP 7: ACTION :  Execute WebPa GET command on the object Device.WiFi");
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
	    LOGGER.info("STEP 11: ACTION :  Execute WebPa GET command on the object Device.WiFi");
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
	    LOGGER.info("STEP 16: ACTION :  Execute WebPa GET command on the object Device.WiFi");
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
	    LOGGER.info("STEP 20: ACTION :  Execute WebPa GET command on the object Device.WiFi");
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
}
