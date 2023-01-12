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

package com.automatics.rdkb.tests.webgui;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientInfo;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiElements;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiTestConstant;
import com.automatics.rdkb.webui.page.LanSideBasePage;
import com.automatics.rdkb.webui.page.LanSidePageNavigation;
import com.automatics.rdkb.webui.page.LanWebGuiLoginPage;
import com.automatics.rdkb.webui.utils.BroadBandWebUiUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.webui.page.BroadBandCommonPage;
import com.automatics.rdkb.webui.page.BroadBandDiagnosticToolsPage;
import com.automatics.rdkb.webui.page.BroadbandLocalIpConfigurationPage;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;

/**
 * Test class for Validating software details from Broad band WebGUi
 * 
 * @author Gnanaprakasham S
 */

public class BroadBandWebGuiTests extends AutomaticsTestBase {



    boolean isBrowserOpen = false; // Boolean variable to check whether Browser is open.
    protected WebDriver driver;
    boolean isDhcpV4Configured = false; // Boolean variable to check whether DHCPV4 Configuration changes are done.
    String errorMessage = null; // Variable to store error message.
    String stepNum = "";
    /** String for step number */
    public int stepNumber = 0;

    /**
     * Test To verify local UI no longer supports the ability to save/restore the config file from Admin page
     * 
     * <ol>
     * <li>Step 1.Login to the Lan GUI page with correct password and username.</li>
     * <li>step2.check if SAVE CURRENT CONFIGURATION button available in Gateway > At a Glance page.</li>
     * <li>step3.check if Restore Saved Configuration button available in Gateway > At a Glance page</li>
     * 
     * </ol>
     * 
     * @author Deepa Bada
     * @refactor Athira
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBUI-CONFIG-5003")
    public void testToVerifyRestoreOrSaveConfigUnavailabilityInLan(Dut device) {


	// Test case id
	String testCaseId = "TC-RDKB-WEBUI-CONFIG-503";
	// Test step number
	String stepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// String to store the test case status
	boolean status = false;
	String isCaptivePortal = null;
	WebDriver webDriver = null;
	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION 1: Disable captive portal ");
	    LOGGER.info(
		    "PRE-CONDITION 1: ACTION : Execute below WebPA set command - \"Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable\",\"value\":false}]}'");
	    LOGGER.info(
		    "PRE-CONDITION 1: EXPECTED : WebPA request should return success message with the value of captive portal set.");

	    isCaptivePortal = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE);

	    if (CommonMethods.isNotNull(isCaptivePortal)
		    && BroadBandTestConstants.FALSE.equalsIgnoreCase(isCaptivePortal)) {
		LOGGER.info("PRE-CONDITION 1: ACTUAL : Verified Captive portal enable status is FALSE");
	    } else {

		boolean preCondition = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
			BroadBandTestConstants.FALSE);
		LOGGER.info("PRE-CONDITION 1: ACTUAL : " + (preCondition ? "Successfully disabled Captive portal "
			: "Failed to disable Captive portal through WebPA parameter "
				+ BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE));
		if (!preCondition) {
		    throw new TestException("Failed to Disable captive Portal using WebPA request. "
			    + BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE);
		}
	    }

	    LOGGER.info(
		    "PRE-CONDITION 2: DESCRIPTION : Verify whether any device is connected through wifi to the Broadband Gateway");
	    LOGGER.info(
		    "PRE-CONDITION 2: EXPECTED : Broadband Gateway should be connected to the client device through wifi");
	    Dut clientDut = BroadBandConnectedClientUtils
		    .get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);

	    status = (null != clientDut);
	    if (status) {
		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info(
			"PRE-CONDITION 2: ACTUAL :  : Device has been connected with 2.4/5 GHZ private WI-FI network");
	    } else {
		LOGGER.error("PRE-CONDITION 2: ACTUAL : " + errorMessage);
		throw new TestException("Unable to obtain a 2.4 GHz or 5GHz  Working Wi-Fi connected client!");
	    }

	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-CONFIG-5003");
	    LOGGER.info("TEST DESCRIPTION:Remove ability to upload/download config file from the local UI");
	    LOGGER.info("STEP 1 : DESCRIPTION:Login to the Lan GUI page with correct password and username.");
	    LOGGER.info("STEP 2: DESCRIPTION :check for the SAVE CURRENT CONFIGURATION button from GUI.");
	    LOGGER.info("STEP 3:check for the Restore Saved Configuration button from GUI");
	    LOGGER.info("#######################################################################################");

	    /**
	     * step 1 : Launch the Admin page: Device should launch the admin page successfully .
	     */

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 1: DESCRIPTION :Launch Admin Login page and Login");
	    LOGGER.info("STEP 1: ACTION : Login to admin page");
	    LOGGER.info("STEP 1: EXPECTED: Login should be successfull and Redirected to At a Glance Page");
	    LOGGER.info("#######################################################################################");

	    errorMessage = "Unable to Login to LanGUI page using Admin credential";

	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, clientDut);
	    webDriver = LanSideBasePage.getDriver();
	    LOGGER.info("STEP 1: ACTUAL :  " + (status ? "Successfully logged into admin page" : errorMessage));

	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNumber,
		    status, errorMessage, true);

	    /**
	     * Step 2 :check if "SAVE CURRENT CONFIGURATION" button available in "Gateway > At a Glance" page
	     */

	    LOGGER.info("######################################################");
	    LOGGER.info(
		    "STEP 2 : DESCRIPTION:  check if SAVE CURRENT CONFIGURATION button available in Gateway > At a Glance page");
	    LOGGER.info("STEP 2 : ACTION: check for the SAVE CURRENT CONFIGURATION button from GUI.");
	    LOGGER.info("STEP 2 : EXPECTED: SAVE CURRENT CONFIGURATION button must not present");
	    LOGGER.info("######################################################");
	    stepNumber = "s2";
	    status = false;
	    errorMessage = "Unable to verify SAVE CURRENT CONFIGURATION button existence";
	    status = BroadBandWebUiUtils.verifySaveOrRestoreConfigurationsButtonExists(
		    BroadBandWebGuiElements.ELEMENT_XPATH_SAVE_CURRENT_CONFIGURATIONS, webDriver);

	    LOGGER.info("STEP 2: ACTUAL :  "
		    + (status ? "Verified SAVE CURRENT CONFIGURATION button is not present" : errorMessage));

	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNumber,
		    status, errorMessage, true);

	    /**
	     * Step 3 :check if "Restore Saved Configuration" button available in "Gateway > At a Glance" page
	     */
	    LOGGER.info("######################################################");
	    LOGGER.info(
		    "STEP 3 : DESCRIPTION:  check if Restore Saved Configuration button available in Gateway > At a Glance page");
	    LOGGER.info("STEP 3 : ACTION: check for the Restore Saved Configuration button from GUI.");
	    LOGGER.info("STEP 3 : EXPECTED: Restore Saved Configuration button must not present");
	    LOGGER.info("######################################################");
	    stepNumber = "s3";
	    status = false;
	    errorMessage = "unable to verify Restore Saved Configuration button avialability ";
	    status = BroadBandWebUiUtils.verifySaveOrRestoreConfigurationsButtonExists(
		    BroadBandWebGuiElements.ELEMENT_XPATH_RESTORE_SAVED_CONFIGURATIONS, webDriver);

	    LOGGER.info("STEP 3: ACTUAL :  "
		    + (status ? "Verified Restore Saved Configuration button is not present" : errorMessage));

	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNumber,
		    status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURED WHILE VALIDATING RESTORE/SAVE CONFIG UNAVILABILITY IN LAN GUI" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Change captive portal value to default using WebPA");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : Execute webpa set request for the parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable\"");
	    LOGGER.info(
		    "POST-CONDITION : EXPECTED : WebPA request should return success and default value should be set for Gateway IP");
	    boolean postCondition = false;
	    if (CommonMethods.isNotNull(isCaptivePortal)) {
		postCondition = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
			isCaptivePortal);
	    } else {
		LOGGER.info("Captive portal value obtained as null");
	    }
	    LOGGER.info("POST-CONDITION : ACTUAL : "
		    + (postCondition ? "Post condition executed successfully" : "Post condition failed"));
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-CONFIG-5003");

    }

    /**
     * Verify the details of connected clients can be retrieved from User admin page
     * <ol>
     * <li>Connect the client 1 to Private Wi-Fi Network and verify connection status</li>
     * <li>Verify the IPv4 Address is retrieved from the client connected to Private Wi-Fi Network</li>
     * <li>Verify the IPv6 Address is retrieved from the client connected to Private Wi-Fi Network</li>
     * <li>Verify the internet is accessible in the client connected to Private Wi-Fi Network</li>
     * <li>Verify the Wifi Mac address of wireless client is retrieved successfully</li>
     * <li>Verify the Wired client connected to ethernet has been retrieved successfully</li>
     * <li>Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP</li>
     * <li>Verify the IPv6 Address is retrieved from the client connected to Ethernet</li>
     * <li>Verify the internet is accessible in the client connected to Ethernet</li>
     * <li>Verify the Mac address of wired client is retrieved successfully</li>
     * <li>Launch Broad band User Admin page login page and verify login status</li>
     * <li>Launch the Connected Devices page from Gateway > At a Glance page</li>
     * <li>Verify the IP Address, Physical Address and Connection Type of all the online Devices listed can be retrieved
     * from User admin page</li>
     * <li>Cross-verify the IP Address, Physical MAC Address & Connection Type of the wired and wireless clients
     * retrieved from User Admin page</li>
     * </ol>
     * 
     * @param device
     *            {@link Instanceof Dut}
     * @author SATHYA KISHORE
     * @refactor Athira
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-CON-DEV-5001")
    public void testToVerifyConnectedClientDetailsInUserAdminPage(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "s1";
	String errorMessage = "";
	boolean status = false;
	Dut connectedClientDut = null;
	Dut wiredConnectedClientDut = null;
	List<String> ipAddressesRetrievedFromWirelessClient = null;
	List<String> ipAddressesRetrievedFromWiredClient = null;
	boolean isBrowserOpen = false;
	WebDriver webDriver = null;
	BroadBandConnectedClientInfo wirelessClient = new BroadBandConnectedClientInfo();
	BroadBandConnectedClientInfo wiredClient = new BroadBandConnectedClientInfo();
	List<BroadBandConnectedClientInfo> listOfActiveDevices = new ArrayList<BroadBandConnectedClientInfo>();
	List<BroadBandConnectedClientInfo> listOfActiveDevicesRetrievedFromWebGui = null;
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-CON-DEV-501";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CON-DEV-5001");
	LOGGER.info("TEST DESCRIPTION: Verify the details of connected clients can be retrieved from User admin page");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Connect the client 1 to Private Wi-Fi Network and verify connection status ");
	LOGGER.info("2. Verify the IPv4 Address is retrieved  from the client connected to Private Wi-Fi Network");
	LOGGER.info("3. Verify the IPv6 Address is retrieved  from the client connected to Private Wi-Fi Network");
	LOGGER.info("4. Verify the internet is accessible in the client connected to Private Wi-Fi Network");
	LOGGER.info("5. Verify the Wifi Mac address of wireless client is retrieved successfully");
	LOGGER.info("6. Verify the Wired client connected to ethernet has been retrieved successfully");
	LOGGER.info("7. Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP");
	LOGGER.info("8. Verify the IPv6 Address is retrieved  from the client connected to Ethernet");
	LOGGER.info("9. Verify the internet is accessible in the client connected to Ethernet");
	LOGGER.info("10. Verify the Mac address of wired client is retrieved successfully");
	LOGGER.info("11. Launch Broad band User Admin page login page and verify login status");
	LOGGER.info("12. Launch the Connected Devices page from Gateway > At a Glance page");
	LOGGER.info(
		"13. Verify the IP Address, Physical Address and Connection Type of all the online Devices listed can be retrieved from User admin page");
	LOGGER.info(
		"14. Cross-verify the IP Address, Physical MAC Address & Connection Type of the wired and wireless clients retrieved from User Admin page");
	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "S1";
	    errorMessage = "Unable to connect to the private Wi-Fi Network";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Connect the client 1 to Private Wi-Fi Network and verify connection status ");
	    LOGGER.info(
		    "STEP 1: ACTION : Connect to Private Wi-Fi using below commandsLinux :nmcli dev wifi connect <ssid> password <passwd>Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP 1: EXPECTED : Device should be connected with  Private Wi-Fi Network");
	    LOGGER.info("**********************************************************************************");
	    try {
		connectedClientDut = BroadBandConnectedClientUtils
			.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = null != connectedClientDut;
	    boolean isWirelessClientAvailable = status;
	    if (status) {
		wirelessClient.setConnectionType(BroadBandTestConstants.WIFI);
		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info("S1 ACTUAL: Client 1 has been connected with the Private Wi-Fi Network successfully");
	    } else {
		LOGGER.error("S1 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    if (isWirelessClientAvailable) {

		// step-2 to step-4
		ipAddressesRetrievedFromWirelessClient = BroadBandConnectedClientUtils
			.validateIpAddressesAndInternetConnectivityOfConnectedClient(device, connectedClientDut, tapEnv,
				BroadBandTestConstants.CONSTANT_2, testCaseId);
		for (String ipAddress : ipAddressesRetrievedFromWirelessClient) {
		    if (CommonMethods.isIpv4Address(ipAddress)) {
			wirelessClient.setIpv4Address(ipAddress);
		    }
		}

		stepNum = "S5";
		errorMessage = "Unable to retrieve the Physical Mac address of the client connected to Private Wi-Fi";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 5: DESCRIPTION : Verify the Wifi Mac address of wireless client is retrieved successfully");
		LOGGER.info("STEP 5: ACTION : Execute the command in the connected client :  ipconfig /all ");
		LOGGER.info("STEP 5: EXPECTED : Mac address of the Wi-Fi adapter should be retrieved successfully");
		LOGGER.info("**********************************************************************************");
		String macAddressOfWirelessClient = BroadBandConnectedClientUtils
			.getIpOrMacFromWindowsConnectedClient(connectedClientDut, tapEnv, false);
		LOGGER.info("Physical Mac Address of the client connected to Private Wi-Fi is : "
			+ macAddressOfWirelessClient);
		status = CommonMethods.isNotNull(macAddressOfWirelessClient)
			&& CommonMethods.isMacValid(macAddressOfWirelessClient);
		if (status) {
		    macAddressOfWirelessClient = macAddressOfWirelessClient
			    .replace(BroadBandTestConstants.COLON, BroadBandTestConstants.EMPTY_STRING).toUpperCase();
		    wirelessClient.setMacAddress(macAddressOfWirelessClient);
		    listOfActiveDevices.add(wirelessClient);
		    LOGGER.info(
			    "S5 ACTUAL : Successfully retrieved the Physical Mac address of the client connected to Private Wi-Fi");
		} else {
		    LOGGER.error("S5 ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    } else {
		LOGGER.error(
			"Unable to connect to Private Wi-Fi network, hence skipping S2 to S5 which invloves validation using Wi-Fi client.");
	    }

	    stepNum = "S6";
	    errorMessage = "There is no client associated with the gateway through Ethernet";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify the Wired client connected to ethernet has been retrieved successfully");
	    LOGGER.info(
		    "STEP 6: ACTION : Get the ethernet client from the list of clients associated with the account");
	    LOGGER.info("STEP 6: EXPECTED : Client connected to the Ethernet should be retrieved successfully");
	    LOGGER.info("**********************************************************************************");
	    wiredConnectedClientDut = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
	    status = null != wiredConnectedClientDut;
	    boolean isWiredClientAvailable = status;
	    if (status) {
		wiredClient.setConnectionType(BroadBandTestConstants.CONNECTION_TYPE_ETHERNET);
		LOGGER.info("S6 ACTUAL: Client connected through ethernet has been retrieved successfully");
	    } else {
		LOGGER.error("S6 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    if (isWiredClientAvailable) {

		// step-7 to step-9
		ipAddressesRetrievedFromWiredClient = BroadBandConnectedClientUtils
			.validateIpAddressesAndInternetConnectivityOfConnectedClient(device, wiredConnectedClientDut,
				tapEnv, BroadBandTestConstants.CONSTANT_7, testCaseId);

		for (String ipAddress : ipAddressesRetrievedFromWiredClient) {
		    if (CommonMethods.isIpv4Address(ipAddress)) {
			wiredClient.setIpv4Address(ipAddress);
		    }
		}

		stepNum = "S10";
		errorMessage = "Unable to retrieve the Physical Mac address of the client connected to Ethernet";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 10: DESCRIPTION : Verify the Mac address of wired client is retrieved successfully");
		LOGGER.info("STEP 10: ACTION : Execute the command in the connected client :  ipconfig /all ");
		LOGGER.info("STEP 10: EXPECTED : Mac address of the Ethernet client should be retrieved successfully");
		LOGGER.info("**********************************************************************************");
		String macAddressOfWiredClient = BroadBandConnectedClientUtils
			.getIpOrMacFromLinuxConnectedClient(device, wiredConnectedClientDut, tapEnv, false);
		LOGGER.info("Physical Mac Address of the client connected to Ethernet is : " + macAddressOfWiredClient);
		status = CommonMethods.isNotNull(macAddressOfWiredClient)
			&& CommonMethods.isMacValid(macAddressOfWiredClient);
		if (status) {
		    macAddressOfWiredClient = macAddressOfWiredClient
			    .replace(BroadBandTestConstants.COLON, BroadBandTestConstants.EMPTY_STRING).toUpperCase();
		    wiredClient.setMacAddress(macAddressOfWiredClient);
		    listOfActiveDevices.add(wiredClient);
		    LOGGER.info(
			    "S10 ACTUAL : Successfully retrieved the Physical Mac address of the client connected to Ethernet");
		} else {
		    LOGGER.error("S10 ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    } else {
		LOGGER.error(
			"There is no client associated with the gateway through Ethernet , hence skipping S7 to S10 which invloves validation using Wi-Fi client.");
	    }

	    if (isWiredClientAvailable || isWirelessClientAvailable) {

		stepNum = "S11";
		errorMessage = "Unable to login gateway admin page";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 11: DESCRIPTION : Launch Broad band User Admin page login page and verify login status");
		LOGGER.info("STEP 11: ACTION : Naviagte to AdminUI login page using admin/password credentials");
		LOGGER.info(
			"STEP 11: EXPECTED : Gateway > At a Glance page should be launched and login should be successful using admin credentials");
		LOGGER.info("**********************************************************************************");
		try {
		    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device,
			    (isWirelessClientAvailable ? connectedClientDut : wiredConnectedClientDut));
		    isBrowserOpen = status;
		    webDriver = LanWebGuiLoginPage.getDriver();
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred during Gateway Admin Page login : " + exception.getMessage());
		}
		if (status) {
		    LOGGER.info(
			    "STEP 11: ACTUAL : Launch Broad band User Admin Page and verify login status is successful");
		} else {
		    LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
			status, errorMessage, true);

		stepNum = "S12";
		errorMessage = "Unable to navigate Connected Devices page";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 12: DESCRIPTION : Launch the Connected Devices page from Gateway > At a Glance page");
		LOGGER.info("STEP 12: ACTION : Navigate to Devices  page by clicking on \"Connected Devices\" Menu ");
		LOGGER.info(
			"STEP 12: EXPECTED : Devices details page should get displayed having page title as \"Connected Devices >Devices\"");
		LOGGER.info("**********************************************************************************");
		try {

		    status = LanSideBasePage.isPageLaunchedForPartners(device, tapEnv,
			    BroadBandWebGuiTestConstant.LINK_TEXT_CONNECTED_DEVICES,
			    BroadBandTestConstants.CONNECTED_DEVICES);
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred while navigating to connected devices page in LAN  GUI Page: "
			    + exception.getMessage());
		}
		if (status) {
		    LOGGER.info(
			    "STEP 12: ACTUAL : Connected Devices page has been launched successfully having Page Title as Gateway > Connected Devices");
		} else {
		    LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
			status, errorMessage, true);

		stepNum = "S13";
		errorMessage = "Unable to retrieve details of active clients connected to gateway from user Admin Page";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 13: DESCRIPTION : Verify the IP Address, Physical Address and Connection Type of all the online Devices listed can be retrieved from User admin page");
		LOGGER.info(
			"STEP 13: ACTION : Retrieve the details of all the Online devices listed in  \"Connected Devices --> Online Device\" menu");
		LOGGER.info(
			"STEP 13: EXPECTED : Getting the list of Online device\"s  from WebUI should be successful ");
		LOGGER.info("**********************************************************************************");
		try {
		    listOfActiveDevicesRetrievedFromWebGui = BroadBandCommonPage
			    .getDetailsOfActiveDevicesFromGui(webDriver);
		    if (listOfActiveDevicesRetrievedFromWebGui != null) {
			status = listOfActiveDevicesRetrievedFromWebGui.size() >= listOfActiveDevices.size();
			errorMessage = "No of online devices retrieved from WEB GUI do not match with the no of clients associated with the gateway";
		    }
		} catch (Exception ex) {
		    errorMessage = "Exception occurred while retrieving details of online clients from User Admin Page.";
		    LOGGER.error(ex.getMessage());
		}
		if (status) {
		    LOGGER.info(
			    "STEP 13: ACTUAL : All the details of active clients connected to gateway have been retrieved successfully from user Admin page");
		} else {
		    LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
			status, errorMessage, true);

		stepNum = "S14";
		errorMessage = "Connection Details retrieved from User Admin page and Client do not match";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 14: DESCRIPTION : Cross-verify the IP Address, Physical MAC Address & Connection Type of the wired and wireless clients retrieved from User Admin page");
		LOGGER.info(
			"STEP 14: ACTION : Compare the IP Address, Physical MAC Address & Connection Type retrieved from client with the value obtained from User Admin page");
		LOGGER.info(
			"STEP 14: EXPECTED : IP Address, Physical MAC Address & Connection Type retrived from user admin page should be same as the value obtained from client");
		LOGGER.info("**********************************************************************************");
		LOGGER.info("CONNECTION DETAILS FROM CLIENT : " + listOfActiveDevices.toString());
		LOGGER.info("CONNECTION DETAILS FROM WEBGUI: " + listOfActiveDevicesRetrievedFromWebGui.toString());
		int counter = BroadBandTestConstants.CONSTANT_0;
		for (BroadBandConnectedClientInfo clientDataRetrievedFromGui : listOfActiveDevicesRetrievedFromWebGui) {
		    for (BroadBandConnectedClientInfo clientInfo : listOfActiveDevices) {
			if (BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
				clientDataRetrievedFromGui.getMacAddress(), clientInfo.getMacAddress())
				&& BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
					clientDataRetrievedFromGui.getIpv4Address(), clientInfo.getIpv4Address())
				&& CommonUtils.patternSearchFromTargetString(
					clientDataRetrievedFromGui.getConnectionType(),
					clientInfo.getConnectionType())) {
			    counter++;
			    LOGGER.info("COUNTER : " + counter);
			}
		    }
		}
		status = (counter == listOfActiveDevices.size());
		if (status) {
		    LOGGER.info(
			    "STEP 14: ACTUAL : IP Address, Physical MAC Address & Connection Type retrived from user admin page is same as the value obtained from wired & wireless clients");
		} else {
		    LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    } else {
		LOGGER.error("Both Wi-Fi & Ethernet clients are not available , hence skipping the test !");
	    }

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    if (isBrowserOpen) {
		try {
		    LanSideBasePage.closeBrowser();
		    LOGGER.info("Browser closed successfully");
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred while closing the browser, unable to close browser.");
		}
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CON-DEV-5001");
    }

    /**
     * 
     * @param device
     *            instance of {@link Dut}
     * @param wifiSSID
     *            Wifi frequency band
     * @param clientDevice
     *            Connected Client Device
     * @param wifiSsidName
     *            SSID Name
     * @param wifiPassPhrase
     *            Passphrase
     * @param defaultChannel
     *            Default channel
     */
    public void postConfigurationsForWifiRestoration(Dut device, WiFiFrequencyBand wifiSSID, Dut clientDevice,
	    String wifiSsidName, String wifiPassPhrase, String defaultChannel) {
	boolean status = false;
	String errorMessage = null;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Disconnect " + wifiSSID + " from the device");
	    LOGGER.info("POST-CONDITION 1 : ACTION :Disconnect wifi radio " + wifiSSID);
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : Wifi " + wifiSSID + " should be disconnected successfully");
	    LOGGER.info("#######################################################################################");
	    LanWebGuiLoginPage.closeBrowser();
	    errorMessage = "Unable to disconnect wifi " + wifiSSID;
	    String ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    wifiSSID);
	    LOGGER.info("SSIDNAME:" + ssidName);
	    boolean connectionStatus = ConnectedNattedClientsUtils.disconnectSSID(clientDevice, tapEnv, ssidName);
	    LOGGER.info("POST CONDITION 1:ACTUAL: WIFI " + wifiSSID + " Disconnect status:" + connectionStatus);
	    if (connectionStatus) {
		LOGGER.info("POST CONDITION 1:ACTUAL: WIFI " + wifiSSID + " Disconnect status:" + connectionStatus);
	    } else {
		LOGGER.error("POST CONDITION 1:ACTUAL: " + errorMessage);

	    }
	    status = false;
	    errorMessage = "Unable to set " + wifiSSID + " SSID name to default";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 2 : DESCRIPTION :Restore " + wifiSSID + " name using Webpa");
	    LOGGER.info("POST-CONDITION 2 : ACTION :Execute webpa command with Param " + wifiSSID
		    + " SSID with retrieved value");
	    LOGGER.info("POST-CONDITION 2 : EXPECTED : Webpa set should be successful");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to set " + wifiSSID + " SSID name to default";
	    if (CommonMethods.isNotNull(wifiSsidName)) {
		switch (wifiSSID) {
		case WIFI_BAND_2_GHZ:
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
			    BroadBandTestConstants.CONSTANT_0, wifiSsidName,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    break;
		case WIFI_BAND_5_GHZ:
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
			    BroadBandTestConstants.CONSTANT_0, wifiSsidName,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    break;
		default:
		    break;
		}
	    }
	    if (status) {
		LOGGER.info("POST CONDITION 2:ACTUAL: WIFI SSID NAME OF 5GHZ Restore status:" + status);
	    } else {
		LOGGER.error("POST CONDITION 2:ACTUAL: " + errorMessage);

	    }
	    status = false;
	    errorMessage = "Unable to set " + wifiSSID + " SSID passphrase to default";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 3 : DESCRIPTION :Restore" + wifiSSID + " WiFi SSID PassPhrase using Webpa");
	    LOGGER.info("POST-CONDITION 3 : ACTION :Execute webpa command with Param " + wifiSSID
		    + " passphrase with retrieved value ");
	    LOGGER.info("POST-CONDITION 3 : EXPECTED : Webpa set should be successful");
	    LOGGER.info("#######################################################################################");
	    if (CommonMethods.isNotNull(wifiPassPhrase)) {
		switch (wifiSSID) {
		case WIFI_BAND_2_GHZ:
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2GHZ_SECURITY_KEYPASSPHRASE,
			    BroadBandTestConstants.CONSTANT_0, wifiPassPhrase,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    break;
		case WIFI_BAND_5_GHZ:
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SECURITY_KEYPASSPHRASE,
			    BroadBandTestConstants.CONSTANT_0, wifiPassPhrase,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    break;
		default:
		    break;
		}
	    }
	    if (status) {
		LOGGER.info(
			"POST CONDITION 3:ACTUAL: WIFI SSID PASSPHRASE OF " + wifiSSID + " Restore status:" + status);
	    } else {
		LOGGER.error("POST CONDITION 3:ACTUAL: " + errorMessage);

	    }
	    status = false;
	    errorMessage = "Unable to set " + wifiSSID + " SSID channel bandwidth to default";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 4 : DESCRIPTION :Restore " + wifiSSID + " SSID Channel bandwidth using Webpa");
	    LOGGER.info("POST-CONDITION 4 : ACTION :Execute webpa command with Param " + wifiSSID
		    + " channel bandwidth and retrieved value");
	    LOGGER.info("POST-CONDITION 4 : EXPECTED : Webpa set should be successful");
	    LOGGER.info("#######################################################################################");
	    if (CommonMethods.isNotNull(defaultChannel)) {
		switch (wifiSSID) {
		case WIFI_BAND_2_GHZ:
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND,
			    BroadBandTestConstants.CONSTANT_0, defaultChannel,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    break;
		case WIFI_BAND_5_GHZ:
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_5GHZ_BAND,
			    BroadBandTestConstants.CONSTANT_0, defaultChannel,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    break;
		default:
		    break;
		}
	    }
	    if (status) {
		LOGGER.info("POST CONDITION 4:ACTUAL: WIFI SSID Channel bandwidth OF " + wifiSSID + " Restore status:"
			+ status);
	    } else {
		LOGGER.error("POST CONDITION 4:ACTUAL: " + errorMessage);

	    }
	} catch (Exception e) {
	    LOGGER.error("Exception caught in POST CONFIGURATIONS:" + e.getMessage());
	    throw new TestException(BroadBandTestConstants.ERROR_LOG + errorMessage);
	}
    }

    /**
     * Verify the Manage Sites and Managed Services should not accept rule which has Start time greater than End Time
     * <ol>
     * <li>Verify whether Private Wifi SSIDs\" are enabled using WebPA.</li>
     * <li>Connect the client to Private Wi-Fi Network and verify connection status</li>
     * <li>Verify the client connected to Private Wi-Fi Network has got the IPv4 Address between DHCP Range</li>
     * <li>Verify the client connected to Private Wi-Fi Network has got the IPv6 Address</li>
     * <li>Verify the internet is accessible in the client connected to the Private Wi-Fi Network</li>
     * <li>Verify the gateway admin page on Wi-Fi client is launched and logged in successfully</li>
     * <li>Launch the Managed Sites page from Gateway > At a Glance page</li>
     * <li>Verify the Managed Sites feature can be enabled</li>
     * <li>Launch the \"Managed Sites - Add Blocked Domain page</li>
     * <li>Verify the Manage Sites - Add Blocked Domain should not accept rule which has Start time greater than End
     * Time</li>
     * <li>Launch the \"Managed Sites - Add Blocked Keyword page</li>
     * <li>Verify the Manage Sites - Add Blocked Domain should not accept rule which has Start time greater than End
     * Time</li>
     * <li>Verify the Managed Sites feature can be disabled</li>
     * <li>Launch the Managed Services page</li>
     * <li>Verify the Managed Services feature can be enabled</li>
     * <li>Launch the \"Managed Services -Add Blocked Service page</li>
     * <li>Verify the Manage Services - Add Blocked Service should not accept rule which has Start time greater than End
     * Time</li>
     * <li>Verify the Managed Services feature can be disabled</li>
     * </ol>
     * 
     * @author Gnanaprakasham S
     * @refactor Rakesh C N
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-PARE-CTRL-NEG-5001")
    public void testToVerifyParentalControl(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-PARE-CTRL-NEG-501";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-PARE-CTRL-NEG-5001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the Manage Sites and Managed Services should not accept rule which has Start time greater than End Time");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify whether Private Wifi SSIDs\" are enabled using WebPA.");
	LOGGER.info("2. Connect the client to Private Wi-Fi Network and verify connection status ");
	LOGGER.info(
		"3. Verify the client connected to Private Wi-Fi Network has got the IPv4 Address between DHCP Range");
	LOGGER.info("4. Verify the client connected to Private Wi-Fi Network has got the IPv6 Address");
	LOGGER.info("5. Verify the internet is accessible in the client connected to the Private Wi-Fi Network");
	LOGGER.info("6. Verify the gateway admin page on Wi-Fi  client is launched and logged in successfully");
	LOGGER.info("7. Launch the Managed Sites page from Gateway > At a Glance page");
	LOGGER.info("8. Verify the Managed Sites feature can be enabled");
	LOGGER.info("9. Launch the \"Managed Sites - Add Blocked Domain page ");
	LOGGER.info(
		"10. Verify the Manage Sites - Add Blocked Domain should not accept rule which has Start time greater than End Time");
	LOGGER.info("11. Launch the \"Managed Sites - Add Blocked Keyword page ");
	LOGGER.info(
		"12. Verify the Manage Sites - Add Blocked Domain should not accept rule which has Start time greater than End Time");
	LOGGER.info("13. Verify the Managed Sites feature can be disabled");
	LOGGER.info("14. Launch the Managed Services page");
	LOGGER.info("15. Verify the Managed Services feature can be enabled");
	LOGGER.info("16. Launch the \"Managed Services -Add Blocked Service page ");
	LOGGER.info(
		"17. Verify the Manage Services - Add Blocked Service should not accept rule which has Start time greater than End Time");
	LOGGER.info("18. Verify the Managed Services feature can be disabled");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "Enabling Private Wi-Fi SSIDs' via WebPA failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify whether Private Wifi SSIDs\" are enabled using WebPA.");
	    LOGGER.info("STEP 1: ACTION : Execute the command to check Private Wifi SSIDs enabled or not");
	    LOGGER.info("STEP 1: EXPECTED : Both 2.4 GHz and 5 GHz radio should be enabled");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Enabling 2.4 GHz private Wi-Fi radio via WebPA failed";
	    if (BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, true)) {
		status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
			WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
		errorMessage = "Enabling 5 GHz private Wi-Fi radio via WebPA failed";
	    }
	    if (status) {
		LOGGER.info("S1 ACTUAL: Both 2.4 GHz & 5 GHz Private Wi-Fi SSIDs' are enabled using WebPA");
	    } else {
		LOGGER.error("S1 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s2";
	    status = false;
	    Dut wirelessConnectedClientSettop = null;
	    errorMessage = "Unable to connect to the private Wi-Fi Network Or WiFi capable devices are not available";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Connect the client to Private Wi-Fi Network and verify connection status ");
	    LOGGER.info(
		    "STEP 2: ACTION : Connect to the wifi using below commandsLinux :nmcli dev wifi connect <ssid> password <passwd>Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP 2: EXPECTED : Device should be connected with the Private Wi-Fi Network");
	    LOGGER.info("**********************************************************************************");
	    try {
		wirelessConnectedClientSettop = BroadBandConnectedClientUtils
			.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = null != wirelessConnectedClientSettop;
	    if (status) {
		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info("S2 ACTUAL: Device has been connected with the Private Wi-Fi Network");
	    } else {
		LOGGER.error("S2 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    // step-3 to step-5
	    BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
		    wirelessConnectedClientSettop, tapEnv, BroadBandTestConstants.CONSTANT_3, testCaseId);

	    stepNum = "s6";
	    status = false;
	    errorMessage = "Unable to login gateway admin page";
	    WebDriver webDriver = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify the gateway admin page on Wi-Fi  client is launched and logged in successfully");
	    LOGGER.info("STEP 6: ACTION : Launch the URL in Wi-Fi client : http://10.0.0.1");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Gateway admin page on Wi-Fi client should sucessfully get launched and logged in");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, wirelessConnectedClientSettop);
	    } catch (Exception exception) {
		LOGGER.error("Exception occurred during Gateway Admin Page login : " + exception.getMessage());
	    }
	    if (status) {
		webDriver = LanSideBasePage.getDriver();
		LOGGER.info(
			"STEP 6: ACTUAL : Gateway admin page is accessible from the client connected using admin/****** credential for Residential or cusadmin/****** credential for Commercial devices");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s7";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Launch the Managed Sites page from Gateway > At a Glance page");
	    LOGGER.info("STEP 7: ACTION : Navigate to  Parental Control > Managed Sites");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Managed Sites page should be successfully launched with page title as \"Parental Control > Managed Sites \"");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to launch Managed Sites page from Gateway > At a Glance page";
	    status = LanSideBasePage.isPageLaunchedByUsingWebElementforParentalControlManagedSites(device, tapEnv,
		    BroadBandWebGuiTestConstant.LINK_TEXT_PARENTAL_CONTROL,
		    BroadbandPropertyFileHandler.getParentalControlManagedSitesPageTitle());
	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : Managed Sites page is successfully launched with page title as 'Parental Control > Managed Sites'");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s8";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify the Managed Sites feature can be enabled");
	    LOGGER.info("STEP 8: ACTION : Click on \"Enable\" button in Managed sites page");
	    LOGGER.info("STEP 8: EXPECTED : Managed Sites should be successfully enabled");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to enable Managed Sites feature!";
	    status = BroadBandCommonPage.enableOrDisableRadioButton(tapEnv,
		    BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SITES_ENABLE);
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Parental Control - Managed Sites is successfully enabled!");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s9";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Launch the \"Managed Sites - Add Blocked Domain page ");
	    LOGGER.info("STEP 9: ACTION : Navigate to  Parental Control > Managed Sites > Add Blocked Domain");
	    LOGGER.info(
		    "STEP 9: EXPECTED : Managed Sites - Add Blocked Domain page should be successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Domain'");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to launch Managed Sites - Add Blocked Domain from Managed Sites page";
	    status = LanSideBasePage.isPageLaunchedByUsingWebElementforParentalControlAddBlockDomain(device, tapEnv,
		    By.xpath(BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SITES_ADD_SITES),
		    BroadBandTestConstants.MANAGED_SITES_ADD_SITES);
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : Managed Sites - Add Blocked Domain page is successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Domain'");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s10";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Verify the Manage Sites - Add Blocked Domain should not accept rule which has Start time greater than End Time");
	    LOGGER.info(
		    "STEP 10: ACTION : Enter URL, Select 'Block Always' as 'No' and select 'Start From' greater than 'End On' time and click Save Button");
	    LOGGER.info(
		    "STEP 10: EXPECTED : Rule should not be save instead error should be thrown as 'Start time should be smaller than End time !' ");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to validate 'Manage Sites - Add Blocked Domain' rule with Start time greater than End Time";
	    status = BroadBandCommonPage.addParentalControlInvalidRule(tapEnv,
		    BroadBandTestConstants.MANAGED_SITES_ADD_SITES);
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : Rule is not saved, instead error is successfully thrown as 'Start time should be smaller than End time !'");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s11";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Launch the 'Managed Sites - Add Blocked Keyword' page ");
	    LOGGER.info("STEP 11: ACTION : Navigate to  Parental Control > Managed Sites > Add Blocked Keyword");
	    LOGGER.info(
		    "STEP 11: EXPECTED : Managed Sites - Add Blocked Keyword page should be successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Keyword'");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to launch Managed Sites - Add Blocked Keyword from Managed Sites page";
	    status = LanSideBasePage.isPageLaunchedByUsingWebElementforParentalControlAddBlockedKeyword(device, tapEnv,
		    By.xpath(BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SITES_ADD_KEY_WORD),
		    BroadBandTestConstants.MANAGED_SITES_ADD_KEYWORD);
	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : Managed Sites - Add Blocked Keyword page is successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Keyword'");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s12";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify the 'Manage Sites - Add Blocked Keyword' should not accept rule which has Start time greater than End Time");
	    LOGGER.info(
		    "STEP 12: ACTION : Enter Keyword,  Select 'Block Always' as 'No' and select 'Start From' greater than 'End On' time and click Save Button");
	    LOGGER.info(
		    "STEP 12: EXPECTED : Rule should not be save instead error should be thrown as 'Start time should be smaller than End time!' ");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to validate 'Manage Sites - Add Blocked Keyword' rule with Start time greater than End Time";
	    status = BroadBandCommonPage.addParentalControlInvalidRule(tapEnv,
		    BroadBandTestConstants.MANAGED_SITES_ADD_KEYWORD);
	    if (status) {
		LOGGER.info(
			"STEP 12: ACTUAL : Rule is not saved, instead error is successfully thrown as 'Start time should be smaller than End time !'");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s13";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Verify the Managed Sites feature can be disabled");
	    LOGGER.info("STEP 13: ACTION : Click on \"Disable\" button in Managed sites page");
	    LOGGER.info("STEP 13: EXPECTED :  Managed Sites should be successfully enabled");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to disable Managed Sites feature!";
	    status = BroadBandCommonPage.enableOrDisableRadioButton(tapEnv,
		    BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SITES_DISABLE);
	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Parental Control - Managed Sites is successfully disabled!");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s14";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Launch the Managed Services page");
	    LOGGER.info("STEP 14: ACTION : Navigate to Parental Control > Managed Services");
	    LOGGER.info(
		    "STEP 14: EXPECTED : Managed Services page should be successfully launched with page title as 'Parental Control > Managed Services'");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to launch Managed Services page from Managed Sites page";
	    status = LanSideBasePage.isPageLaunchedByUsingWebElementforParentalControlManagedService(device, tapEnv,
		    BroadBandWebGuiTestConstant.LINK_TEXT_MANAGED_SERVICES, BroadBandTestConstants.MANAGED_SERVICES);
	    if (status) {
		LOGGER.info(
			"STEP 14: ACTUAL : Managed Services page is successfully launched with page title as 'Parental Control > Managed Services'");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s15";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify the Managed Services feature can be enabled");
	    LOGGER.info("STEP 15: ACTION : Click on 'Enable' button in Managed Service page");
	    LOGGER.info("STEP 15: EXPECTED :  Managed Services should be successfully enabled");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to enable Managed Services feature!";
	    status = BroadBandCommonPage.enableOrDisableRadioButton(tapEnv,
		    BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SERVICES_ENABLE);
	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : Parental Control - Managed Services is successfully enabled!");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s16";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Launch the 'Managed Services - Add Blocked Service' page ");
	    LOGGER.info("STEP 16: ACTION : Navigate to  Parental Control > Managed Services > Add Blocked Service");
	    LOGGER.info(
		    "STEP 16: EXPECTED : Managed Services - Add Blocked Service page should be successfully launched with page title as 'Parental Control > Managed Services > Add Blocked Service'");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to launch Managed Services - Add Blocked Service from Managed Services page";
	    status = LanSideBasePage.isPageLaunchedByUsingWebElementAddBlockedService(device, tapEnv,
		    By.xpath(BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SERVICE_ADD_SERVICES),
		    BroadBandTestConstants.MANAGED_SERVICES_ADD_SERVICE);
	    if (status) {
		LOGGER.info(
			"STEP 16: ACTUAL : Managed Sites - Add Blocked Service page is successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Keyword'");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s17";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 17: DESCRIPTION : Verify the Manage Services - Add Blocked Service should not accept rule which has Start time greater than End Time");
	    LOGGER.info(
		    "STEP 17: ACTION : Enter keyword,  Select 'Block Always' as 'No' and select 'Start From' greater than 'End On' time and click Save Button");
	    LOGGER.info(
		    "STEP 17: EXPECTED : Rule should not be save instead error should be thrown as 'Start time should be smaller than End time !' ");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to validate 'Manage Services - Add Blocked Service' rule with Start time greater than End Time";
	    status = BroadBandCommonPage.addParentalControlInvalidRule(tapEnv, BroadBandTestConstants.MANAGED_SERVICES);
	    if (status) {
		LOGGER.info(
			"STEP 17: ACTUAL : Rule is not saved, instead error is successfully thrown as 'Start time should be smaller than End time !'");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "s18";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION : Verify the Managed Services feature can be disabled");
	    LOGGER.info("STEP 18: ACTION : Click on \"Disable\" button in Managed Service page");
	    LOGGER.info("STEP 18: EXPECTED :  Managed Services should be successfully enabled");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to disable Managed Services feature !";
	    status = BroadBandCommonPage.enableOrDisableRadioButton(tapEnv,
		    BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SERVICE_DISABLE);
	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Parental Control - Managed Services is successfully disabled!");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-PARE-CTRL-NEG-5001");
    }

    /**
     * Method to verify setting Ethernet client ip as static ip and verify internet connectivity
     * 
     * <ol>
     * <li>Verify obtaining a Ethernet client associated with the gateway.</li>
     * <li>Verify the correct Ipv4 address for LAN Client.</li>
     * <li>Verify the correct Ipv6 address for LAN Client.</li>
     * <li>Verify the internet connectivity in the connected LAN client.</li>
     * <li>Verify retrieving the host name & Ipv4 address of wireless client.</li>
     * <li>Verify launching BroadBand Lan UI login page and verify and login status.</li>
     * <li>Verify navigating to connected device page.</li>
     * <li>Verify navigating to Connected Devices > Devices > Edit Device.</li>
     * <li>Verify adding client ip as static ip.</li>
     * <li>Verify the internet connectivity in the connected LAN client after setting client ip as static.</li>
     * <li>Verify rebooting the device.</li>
     * <li>Verify client ip is set as static ip or not.</li>
     * </ol>
     * 
     * @param device{@link
     *            Dut}
     * 
     * @author Prashant Mishra
     * 
     * @Refactor Sruthi Santhosh
     */
    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true)
    @TestDetails(testUID = "TC-RDKB-WEBUI-8028")
    public void testToValidateSettingEClientIpAsStatic(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBUI-028";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	Dut connectedClientSettop = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-8028");
	LOGGER.info("TEST DESCRIPTION: Verify setting LAN Client ip as static ip and verify internet connectivity.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify connecting a LAN client associated with the gateway.");
	LOGGER.info("2. Verify the correct Ipv4 address for LAN Client.");
	LOGGER.info("3. Verify the correct Ipv6 address for LAN Client.");
	LOGGER.info("4. Verify the internet connectivity in the connected LAN client.");
	LOGGER.info("5. Verify retrieving the host name & Ipv4 address of LAN client.");
	LOGGER.info("6. Verify launching BroadBand Lan UI login page and verify and login status.");
	LOGGER.info("7. Verify navigating to connected device page.");
	LOGGER.info("8. Verify navigating to Connected Devices > Devices > Edit Device.");
	LOGGER.info("9. Verify adding client ip as static ip.");
	LOGGER.info(
		"10. Verify the internet connectivity in the connected LAN client after setting client ip as static.");
	LOGGER.info("11. Verify rebooting the device.");
	LOGGER.info("12. Verify client ip is set as static ip or not.");

	LOGGER.info("#######################################################################################");

	try {
	    stepNum = "s1";
	    errorMessage = "Unable to obtain LAN client.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify obtaining a LAN client associated with the gateway.");
	    LOGGER.info("STEP 1: ACTION : Obtain one of the associated LAN client.");
	    LOGGER.info("STEP 1: EXPECTED : One of the associated LAN client should be obtained successfully.");
	    LOGGER.info("**********************************************************************************");
	    try {
		connectedClientSettop = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = null != connectedClientSettop;
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : One of associated LAN client obtained successfully.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    // step-2 to step-4
	    BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
		    connectedClientSettop, tapEnv, BroadBandTestConstants.CONSTANT_2, testCaseId);

	    /** Common method for verify setting client ip as static ip */
	    methodToVerifySettingClientIpAsStaticForAllClients(device, connectedClientSettop, tapEnv, testCaseId,
		    BroadBandTestConstants.INTEGER_VALUE_5, BroadBandTestConstants.CLIENT_TYPE_ETH);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-8028");
    }

    /**
     * Common method to verify setting client ip as static ip for both client type (Ethernet & WiFI)
     * 
     * @param device
     *            Dut instance
     * @param connectedClientSettop
     *            Connected client device
     * @param tapEnv
     *            AutomaticsTapApi instance
     * @param testCaseId
     *            Test case id for which method is being called
     * 
     * @author Prashant Mishra
     * 
     * @Refactor Sruthi Santhosh
     */
    public static void methodToVerifySettingClientIpAsStaticForAllClients(Dut device, Dut connectedClientSettop,
	    AutomaticsTapApi tapEnv, String testCaseId, int stepNumber, String clientType) {
	// Variable declararion starts
	String ipv4AddressRetrievedAfterReboot = "";
	String ipv4AddressRetrievedBeforeReboot = "";
	String hostName = null;
	String stepNum = "";
	String errorMessage = "";
	String macAddressOfWirelessClientOrginal = "";
	boolean status = false;
	WebDriver webDriver = null;
	LanSidePageNavigation lanSidePageNavigation = null;
	BroadBandResultObject result = null;
	// Variable declaration ends

	stepNum = "s" + stepNumber;
	errorMessage = "Unable to get Ipv4 address of connected client.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ ": DESCRIPTION : Verify retrieving the host name & Ipv4 address of connected client.");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION : Execute WebPA request for the parameter \"Device.Hosts.Host.{i}.HostName\"");
	LOGGER.info("STEP " + stepNumber
		+ ": EXPECTED : host name & Ipv4 address of connected client should be obtained successfully.");
	LOGGER.info("**********************************************************************************");
	try {
	    ipv4AddressRetrievedBeforeReboot = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
		    device, connectedClientSettop);
	    LOGGER.info("ipv4AddressRetrievedBeforeReboot: " + ipv4AddressRetrievedBeforeReboot);
	    if (CommonMethods.isNotNull(ipv4AddressRetrievedBeforeReboot)) {
		errorMessage = "Unable to get host name  and phy address of connected client.";
		hostName = BroadBandCommonUtils.getConnectedClientDetailUsingHostIpAddress(device, tapEnv,
			ipv4AddressRetrievedBeforeReboot, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_HOST_HOSTNAME);
		LOGGER.info("HOST NAME beofre removal of special char: " + hostName);
		hostName = hostName.replace(BroadBandTestConstants.CHARACTER_HYPHEN,
			BroadBandTestConstants.EMPTY_STRING);
		LOGGER.info("HOST NAME after removal of special char: " + hostName);
		macAddressOfWirelessClientOrginal = BroadBandCommonUtils.getConnectedClientDetailUsingHostIpAddress(
			device, tapEnv, ipv4AddressRetrievedBeforeReboot,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_HOST_PHYSICAL_ADDRESS);
		status = CommonMethods.isNotNull(hostName)
			&& CommonMethods.isNotNull(macAddressOfWirelessClientOrginal);
	    }

	} catch (Exception e) {
	    LOGGER.error("Exception occured while getting the host name and Ip address of the connected client."
		    + errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTUAL : Successfully retrieved the host name, Phy address and Ipv4 address of the connected client.");
	} else {
	    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Unable to launch and login LAN UI page.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ ": DESCRIPTION : Verify launching BroadBand Lan UI login page and verify and login status.");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION : Launch the Web GUI Lan page URL and login with correct credentials.");
	LOGGER.info(
		"STEP " + stepNumber + ": EXPECTED : Login page should be launched and login should be successful.");
	LOGGER.info("**********************************************************************************");
	try {
	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, connectedClientSettop);
	    webDriver = LanWebGuiLoginPage.getDriver();
	    lanSidePageNavigation = new LanSidePageNavigation(webDriver);
	} catch (Exception e) {
	    errorMessage = "Exception occured while launching and login Lan UI login page." + errorMessage
		    + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : Login Lan admin page launched and logged in successfully.");
	} else {
	    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Unable to navigate to Connected device page.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify navigating to connected device page.");
	LOGGER.info("STEP " + stepNumber + ": ACTION : Navigate to Connected device page.");
	LOGGER.info(
		"STEP " + stepNumber + ": EXPECTED : Navigation should be  successfull to Connected devices pages.");
	LOGGER.info("**********************************************************************************");
	try {
	    status = lanSidePageNavigation.navigateToConnectedDevicesPage(device, tapEnv, webDriver);
	} catch (Exception e) {
	    errorMessage = "Exception occured while navigating to connected device page." + errorMessage
		    + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : Navigated successfully to Connected device page.");
	} else {
	    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Unable to navigate to Connected Devices > Devices > Edit Device page.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ ": DESCRIPTION : Verify navigating to Connected Devices > Devices > Edit Device.");
	LOGGER.info("STEP " + stepNumber + ": ACTION : Click on edit button for connected client.");
	LOGGER.info("STEP " + stepNumber
		+ ": EXPECTED : Navigation should be successful to Connected Devices > Devices > Edit Device page.");
	LOGGER.info("**********************************************************************************");
	try {
	    status = LanSideBasePage.isPageLaunchedByUsingWebElementForConnectedDevicesEditDevice(device, tapEnv,
		    By.xpath(BroadBandWebGuiElements.XPATH_ADD_DEVICE_WITH_RESERVED_IP),
		    BroadBandTestConstants.ADD_DEVICE_WITH_RESERVED_IP);
	} catch (Exception e) {
	    errorMessage = "Exception occured while navigating to Connected Devices > Devices > Edit Device page."
		    + errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTUAL : Navigated successfully to Connected Devices > Devices > Edit Device page.");
	} else {
	    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Unable to add client ip as static ip.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify adding client ip as static ip.");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION : Click on reserved ip radio button and enter client ip address in text box.");
	LOGGER.info(
		"STEP " + stepNumber + ": EXPECTED : Client ip should be added as static ip from Lan Admin GUI page.");
	LOGGER.info("**********************************************************************************");
	try {

	    String pageTitleKeyword = BroadbandPropertyFileHandler.getPageTitleForConnectedDevicesEditDevicePage();

	    LOGGER.info("pageTitleKeyword: " + pageTitleKeyword);
	    status = BroadBandCommonPage.configureReservedIpAddress(hostName, macAddressOfWirelessClientOrginal,
		    ipv4AddressRetrievedBeforeReboot, tapEnv, pageTitleKeyword, webDriver);

	} catch (Exception e) {
	    errorMessage = "Exception occured while adding client ip as static ip." + errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : Client ip added successfully as static ip.");
	} else {
	    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Unable to  verify internet connectivity in the connected client after setting client ip as static.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber
		+ ": DESCRIPTION : Verify the internet connectivity in the connected wifi client after setting client ip as static.");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION : Execute following command in connected client: curl -4 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -4 -n 5 google.com");
	LOGGER.info("STEP " + stepNumber
		+ ": EXPECTED : The internet connectivity must be successful in the connected client after changing client ip to static.");
	LOGGER.info("**********************************************************************************");
	try {
	    ipv4AddressRetrievedBeforeReboot = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
		    device, connectedClientSettop);
	    LOGGER.info("ipv4AddressRetrievedBeforeReboot: " + ipv4AddressRetrievedBeforeReboot);
	    long startTime = System.currentTimeMillis();
	    do {
		result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
			connectedClientSettop, BroadBandTestConstants.URL_WIKIPEDIA,
			BroadBandTestConstants.EMPTY_STRING);
		status = result.isStatus();
		errorMessage = result.getErrorMessage();
	    } while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	} catch (Exception e) {
	    errorMessage = "Exception occured while verifying nternet connectivity in the connected wifi client after setting client ip as static.."
		    + errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTUAL : Internet connectivity verified successfully after setting client ip as static.");
	} else {
	    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Unable to reboot the device.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify rebooting the device.");
	LOGGER.info("STEP " + stepNumber + ": ACTION : Reboot the device and wait for device to come up.");
	LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be rebooted and comes up with all processes.");
	LOGGER.info("**********************************************************************************");
	try {
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	} catch (Exception e) {
	    errorMessage = "Exception occured while rebooting the device." + errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTUAL : Device rebooted successfully and came up with all the processes.");
	} else {
	    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Failed to reconnect client to 2.4GHz WiFi.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify client ip is set as static ip or not.");
	LOGGER.info("STEP " + stepNumber + ": ACTION : Get client ipv4 address and compare with ip got before reboot,");
	LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ip address before reboot and after reboot should be same.");
	LOGGER.info("**********************************************************************************");
	try {
	    if (clientType.equals(BroadBandTestConstants.CLIENT_TYPE_WIFI)) {
		result = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
			connectedClientSettop, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
		if (!result.isStatus()) {
		    errorMessage = "Failed to reconnect client to 5GHz WiFi.";
		    result = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
			    connectedClientSettop, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
		}
		if (result.isStatus()) {
		    errorMessage = "Unable to verify the static ip address of client.";
		    ipv4AddressRetrievedAfterReboot = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
			    device, connectedClientSettop);
		    LOGGER.info("ipv4AddressRetrievedAfterReboot: " + ipv4AddressRetrievedAfterReboot);
		    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			    ipv4AddressRetrievedBeforeReboot, ipv4AddressRetrievedAfterReboot);
		}
	    } else {
		errorMessage = "Unable to verify the static ip address of client.";
		ipv4AddressRetrievedAfterReboot = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
			device, connectedClientSettop);
		LOGGER.info("ipv4AddressRetrievedAfterReboot: " + ipv4AddressRetrievedAfterReboot);
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			ipv4AddressRetrievedBeforeReboot, ipv4AddressRetrievedAfterReboot);
	    }
	} catch (Exception e) {
	    errorMessage = "Exception occured while verifying static ip of the client." + errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : Client ip verified successfully as static ip.");
	} else {
	    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
    }

    /**
     * Verify assigning DHCP Server Address while old one in use and with expire lease time
     * <ol>
     * <li>PRE CONDITION 1: Connect the client to 2.4 GHz Wi-Fi Network and verify connection status.</li>
     * <li>Verify the Connected client has got the IPv4 Address between DHCP Range</li>
     * <li>Verify the Connected client has got the valid IPv6 Address</li>
     * <li>Verify the Internet is accessible in the Connected client</li>
     * <li>Launch Broad band WebUI login page and verify login status.</li>
     * <li>Verify Local IP network page is launched in connected client successfully.</li>
     * <li>Verify DHCP Beginning Address configured successfully.</li>
     * <li>Verify DHCP End Address configured successfully.</li>
     * <li>Verify DHCP lease time configured successfully.</li>
     * <li>Verify all DHCP configured value saved successfully.</li>
     * <li>Verify DHCP changed beginning address reflected via WEBPA or not.</li>
     * <li>Verify DHCP changed End address reflected via WEBPA or not.</li>
     * <li>Verify DHCP changed Lease time reflected via WEBPA or not.</li>
     * <li>Verify new assigned IP while previous IP is in use.</li>
     * <li>Verify DHCP lease time configured successfully.</li>
     * <li>Verify DHCP Lease time configured value saved successfully.</li>
     * <li>Verify Client IP when lease time is expired.</li>
     * <li>Verify Connected client is disconnected.</li>
     * <li>Connect the client to 2.4 GHz Wi-Fi Network and verify connection status.</li>
     * <li>Verify Client IP when disconnected & reconnected.</li>
     * <li>POST CONDITION 1: Verify DHCP IPv4 values are set to default.</li>
     * <li>POST CONDITION 2: Verify browser is closed in connected client.</li>
     * </ol>
     * 
     * @param device{@link
     *            Dut}
     * 
     * @author Prashant Mishra
     * @Refactor Sruthi Santhosh
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBUI-7034")
    public void verifyDhcpServerConfiguration(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBUI-734";
	String stepNum = "";
	String errorMessage = "";
	String ip4AddressRetrievedFromClient = "";
	String startingAddressLastPart = "";
	String endAddressLastPart = "";
	boolean status = false;
	boolean isBrowserOpen = false;
	HashMap<String, String> defaultDhcpIpv4ValuesMap = null;
	Dut deviceConnectedWith2GhzWifi = null;
	WebDriver webDriver = null;
	BroadbandLocalIpConfigurationPage localIpPage = null;
	BroadBandResultObject reConnectionStatus = null;
	List<String> dhcpStartAndEndAddress = new ArrayList<>();

	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-7034");
	LOGGER.info(
		"TEST DESCRIPTION: Verify assigning DHCP Server Address while old one in use and with expire lease time");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE CONDITION 1. Connect the client to 2.4 GHz Wi-Fi Network and verify connection status.");
	LOGGER.info("1. Verify the Connected client has got the IPv4 Address between DHCP Range");
	LOGGER.info("2. Verify the Connected client has got the valid IPv6 Address");
	LOGGER.info("3. Verify the internet is accessible in the Connected client");
	LOGGER.info("4. Launch Broad band WebUI login page and verify login status.");
	LOGGER.info("5. Verify Local Ip network page is launched in connected client successfully.");
	LOGGER.info("6. Verify DHCP Begining Address configured successfully.");
	LOGGER.info("7. Verify DHCP End Address configured successfully.");
	LOGGER.info("8. Verify DHCP lease time configured successfully.");
	LOGGER.info("9. Verify all DHCP configured value saved successfully.");
	LOGGER.info("10. Verify DHCP changed beginning address reflected via webpa or not.");
	LOGGER.info("11. Verify DHCP changed End address reflected via webpa or not.");
	LOGGER.info("12. Verify DHCP changed Lease time reflected via webpa or not.");
	LOGGER.info("13. Verify new assigned IP while previous IP is in use.");
	LOGGER.info("14. Verify DHCP lease time configured successfully.");
	LOGGER.info("15. Verify  DHCP Lease time configured value saved successfully.");
	LOGGER.info("16. Verify Client IP when lease time is expired.");
	LOGGER.info("17. Verify Connected client is disconnected.");
	LOGGER.info("18. Connect the client to 2.4 GHz Wi-Fi Network and verify connection status.");
	LOGGER.info("18. Verify Client IP when disconnected & reconnected.");
	LOGGER.info("POST CONDITION 1. Verify DHCP IPv4 values are set to default.");
	LOGGER.info("POST CONDITION 1. Verify browser is closed in connected client.");

	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");

	    errorMessage = "Failed to obtain a 2.4GHz WiFi client associated with the Gateway.";
	    LOGGER.info(
		    "PRE-CONDITION 1 : DESCRIPTION : Connect the client to 2.4 GHz Wi-Fi Network and verify connection status.");
	    LOGGER.info(
		    "PRE-CONDITION 1 : ACTION : Connect to 2.4GHz Wi-Fi using below commands Linux :dmcli dev wifi connect <ssid> password <passwd>; Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("PRE-CONDITION 1 : EXPECTED : Device should be connected with 2.4 GHz Wi-Fi Network.");
	    try {
		deviceConnectedWith2GhzWifi = BroadBandConnectedClientUtils
			.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
		status = null != deviceConnectedWith2GhzWifi;
	    } catch (Exception exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info(
			"PRE-CONDITION 1 : ACTUAL : Obtained a WiFi 2.4GHz client assosiated with the gateway successfully.");
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /* Checking Connected Client IPv4, IPv6 Address and Internet Connectivity */
	    /* Step 1 to Step 3 */
	    BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
		    deviceConnectedWith2GhzWifi, tapEnv, BroadBandTestConstants.CONSTANT_1, testCaseId);

	    /* Getting IPv4 Address of Connected Client */
	    ip4AddressRetrievedFromClient = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv, device,
		    deviceConnectedWith2GhzWifi);

	    LOGGER.info("ip4AddressRetrievedFromClient: " + ip4AddressRetrievedFromClient);

	    stepNum = "S4";
	    errorMessage = "Unable to launch Login page in connected client.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Launch Broad band WebUI login page and verify login status.");
	    LOGGER.info(
		    "STEP 4: ACTION : Launch the below URL format in browser: https://10.0.0.1/ and login with credentials.");
	    LOGGER.info("STEP 4: EXPECTED : Login page should be launched and login should be successful.");
	    LOGGER.info("**********************************************************************************");
	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnectedWith2GhzWifi);
	    isBrowserOpen = status;
	    webDriver = LanWebGuiLoginPage.getDriver();
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Launch Broad band LanUI login page and login status is successful.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S5";
	    errorMessage = "Unable to Launch Local IP Network page from LanUI page ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Launch the Local IP Network page from LanUI page");
	    LOGGER.info(
		    "STEP 5: ACTION : Navigate to  Local IP Network page by clicking on \"Connection\" Menu and then select the submenu \"Local Ip Network\".");
	    LOGGER.info(
		    "STEP 5: EXPECTED : Local IP Network page should get displayed having page title as \"Gateway > Connection > Local IP Configuration\"");
	    LOGGER.info("**********************************************************************************");
	    localIpPage = new BroadbandLocalIpConfigurationPage(webDriver);
	    status = localIpPage.navigateToLocalIpPage(tapEnv, device);
	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : Navigating to Local IP Network page by clicking on \"Connection\" Menu and then select the submenu \"Local Ip Network\" is successful.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S6";
	    errorMessage = "Unable to enter DHCP Begining address in text box.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify DHCP Begining Address configured successfully.");
	    LOGGER.info("STEP 6: ACTION : Enter DHCP Begining address value in DHCP Begining address text box.");
	    LOGGER.info("STEP 6: EXPECTED : DHCP Begining address should be entered successfully in text box.");
	    LOGGER.info("**********************************************************************************");
	    dhcpStartAndEndAddress = BroadBandCommonUtils
		    .getDhcpRangeBasedOnEsistingRange(ip4AddressRetrievedFromClient);
	    startingAddressLastPart = dhcpStartAndEndAddress.get(0);
	    if (CommonMethods.isNotNull(startingAddressLastPart)) {
		status = LanSideBasePage.enterInTextBoxAndValidate(tapEnv, webDriver,
			BroadBandWebGuiElements.XPATH_FOR_FOURTH_BOX_DHCP_BEGINNING_ADDRESS, startingAddressLastPart);
	    }
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : DHCP Begining Address entered successfully in text box.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S7";
	    errorMessage = "Unable to enter DHCP End Address in text box.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify DHCP End Address configured successfully.");
	    LOGGER.info("STEP 7: ACTION : Enter DHCP End Address value in DHCP Begining address Text Box.");
	    LOGGER.info("STEP 7: EXPECTED : DHCP End address should be entered successfully in text box.");
	    LOGGER.info("**********************************************************************************");
	    endAddressLastPart = dhcpStartAndEndAddress.get(1);
	    if (CommonMethods.isNotNull(endAddressLastPart)) {
		status = LanSideBasePage.enterInTextBoxAndValidate(tapEnv, webDriver,
			BroadBandWebGuiElements.XPATH_FOR_FOURTH_BOX_DHCP_END_ADDRESS, endAddressLastPart);
	    }
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : DHCP End Address entered successfully in text box.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S8";
	    errorMessage = "Unable to enter DHCP lease time amount in text box.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify DHCP lease time amount configured successfully.");
	    LOGGER.info("STEP 8: ACTION : Enter DHCP lease time amount value in text Box.");
	    LOGGER.info("STEP 8: EXPECTED : DHCP lease time amount should be entered successfully in text box.");
	    LOGGER.info("**********************************************************************************");
	    status = LanSideBasePage.enterInTextBoxAndValidate(tapEnv, webDriver,
		    BroadBandWebGuiElements.XPATH_FOR_DHCP_LEASE_TIME_AMOUNT, BroadBandTestConstants.STRING_VALUE_TWO);
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : DHCP lease time amount entered successfully in text box.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S9";
	    errorMessage = "Unable to select DHCP lease time measure from dropdown.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify DHCP lease time measure configured successfully.");
	    LOGGER.info("STEP 9: ACTION : Select DHCP lease time measure value from dropdown.");
	    LOGGER.info("STEP 9: EXPECTED : DHCP lease time amount should be entered successfully in text box.");
	    LOGGER.info("**********************************************************************************");
	    status = LanSideBasePage.selectAndValidateValueFromDropDown(
		    BroadBandWebGuiElements.XPATH_FOR_DHCP_LEASE_TIME_MEASURE,
		    BroadBandTestConstants.DHCP_DROP_DOWN_MINUTES);
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : DHCP lease time amount entered successfully in text box.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S10";
	    errorMessage = "Failed to verify changed DHCP beginning address via webpa.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify DHCP changed beginning address reflected via webpa or not.");
	    LOGGER.info(
		    "STEP 10: ACTION : Click on save button and Execute the webpa command to change DHCP beginning address");
	    LOGGER.info("STEP 10: EXPECTED : Changed DHCP beginning address must reflect via webpa.");
	    LOGGER.info("**********************************************************************************");
	    /** Clicking on 'Save Settings' button */
	    LanSideBasePage.click(By.xpath(BroadBandWebGuiElements.XPATH_FOR_SAVE_BUTTON_DHCP_CONFIGURATIONS));
	    /** Waiting for 1 Minute to reflect changes */
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    String startingAddressAfterDhcpConfiguration = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_MINADDRESS);
	    if (CommonMethods.isNotNull(startingAddressAfterDhcpConfiguration)) {
		status = BroadBandCommonUtils.verifyConfiguredDhcpAddress(startingAddressAfterDhcpConfiguration,
			startingAddressLastPart);
	    }
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : DHCP beginning address changes reflected successfully.");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S11";
	    errorMessage = "Failed to verify changed DHCP End address via webpa.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify DHCP changed End address reflected via webpa or not.");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the webpa command : curl -X GET -H \"Authorization: Bearer <SAT token>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i <WEBPA URL>/v2/device/mac:<CM_MAC>/config?names=Device.DHCPv4.Server.Pool.1.MaxAddress");
	    LOGGER.info("STEP 11: EXPECTED : Changed DHCP End address must reflect via webpa.");
	    LOGGER.info("**********************************************************************************");
	    String endAddressAfterDhcpConfiguration = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_MAXADDRESS);
	    if (CommonMethods.isNotNull(endAddressAfterDhcpConfiguration)) {
		status = BroadBandCommonUtils.verifyConfiguredDhcpAddress(endAddressAfterDhcpConfiguration,
			endAddressLastPart);
	    }
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : DHCP End address changes reflected successfully.");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S12";
	    errorMessage = "Failed to verify changed DHCP Lease time via webpa.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Verify DHCP changed Lease time reflected via webpa or not.");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute the webpa command : curl -X GET -H \"Authorization: Bearer <SAT token>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i <WEBPA URL>/v2/device/mac:<CM_MAC>/config?names=Device.DHCPv4.Server.Pool.1.LeaseTime");
	    LOGGER.info("STEP 12: EXPECTED : Changed DHCP End address must reflect via webpa.");
	    LOGGER.info("**********************************************************************************");
	    String LeaseTimeAfterDhcpConfiguration = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME);
	    if (CommonMethods.isNotNull(endAddressAfterDhcpConfiguration)) {
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			BroadBandTestConstants.STRING_LEASE_TIME_VALUE, LeaseTimeAfterDhcpConfiguration);
	    }
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : DHCP Lease time changes reflected successfully.");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S13";
	    errorMessage = "Unable to connect same client to 2.4GHz Wi-Fi.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : Verify same Client get connected to 2.4GHz Wi-Fi and Local IP Page reloaded.");
	    LOGGER.info(
		    "STEP 13: ACTION : Connect same client to 2.4GHz Wi-Fi based on MAC Address and reload Local IP Page.");
	    LOGGER.info(
		    "STEP 13: EXPECTED : Same Client should be Connected to 2.4GHz Wi-Fi and Local IP Page should be reloaded.");
	    LOGGER.info("**********************************************************************************");
	    LanSidePageNavigation lanSidePageNavigation = new LanSidePageNavigation(webDriver);
	    reConnectionStatus = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
		    deviceConnectedWith2GhzWifi, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    status = reConnectionStatus.isStatus();
	    errorMessage = reConnectionStatus.getErrorMessage();
	    LOGGER.info("Waiting for 30 Seconds after reconnecting to Wi-Fi.");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    LOGGER.info("Page Title after refreshing page: "
		    + BroadbandPropertyFileHandler.getPageTitleForLocalIpNetwork());
	    if (status) {
		try {
		    LOGGER.info("Refreshing and Validating local IP page title.");
		    long startTime = System.currentTimeMillis();
		    do {
			/** Refreshing Local IP Page */
			LOGGER.info("IP page Refresh");
			lanSidePageNavigation.refresh();
			status = BroadBandWebUiUtils.validatePageLaunchedStatusWithPageTitle(webDriver,
				BroadbandPropertyFileHandler.getPageTitleForLocalIpNetwork());

		    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
			    && !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				    BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS));

		} catch (Exception e) {
		    errorMessage = errorMessage + e.getMessage();
		    LOGGER.error(errorMessage);
		}
	    }

	    LOGGER.info("Status is: " + status);
	    if (status) {
		LOGGER.info(
			"STEP 13: ACTUAL : Same Client Connected to 2.4GHz Wi-Fi successfully and Local IP page reloaded successfully.");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S14";
	    errorMessage = "Unable to verify new assigned IP.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Verify new assigned IP while previous IP is in use.");
	    LOGGER.info(
		    "STEP 14: ACTION : Get the device IPv4 address using below commandLinux : ifconfig wlan0\\eth0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Wireless\\Ethernet LAN adapter Wi-Fi\" |grep -i \"IPv4 Address\"");
	    LOGGER.info("STEP 14: EXPECTED : Connected client IP should be changed and within DHCP Range.");
	    LOGGER.info("Waiting for 2 minutes Lease time.");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
		    deviceConnectedWith2GhzWifi);
	    if (status) {
		LOGGER.info(
			"STEP 14: ACTUAL : New Assigned Ip after change in DHCP configuration reflected successfully.");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S15";
	    errorMessage = "Unable to enter new DHCP lease time amount in text box.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify new DHCP lease time amount configured successfully.");
	    LOGGER.info("STEP 15: ACTION : Enter new DHCP lease time amount value in text Box.");
	    LOGGER.info("STEP 15: EXPECTED : new DHCP lease time amount should be entered successfully in text box.");
	    LOGGER.info("**********************************************************************************");
	    status = LanSideBasePage.enterInTextBoxAndValidate(tapEnv, webDriver,
		    BroadBandWebGuiElements.XPATH_FOR_DHCP_LEASE_TIME_AMOUNT, BroadBandTestConstants.STRING_6);
	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : New DHCP lease time amount entered successfully in text box.");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S16";
	    errorMessage = "Unable to select new DHCP lease time measure from dropdown.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Verify new DHCP lease time measure configured successfully.");
	    LOGGER.info("STEP 16: ACTION : Select new DHCP lease time measure value from dropdown.");
	    LOGGER.info("STEP 16: EXPECTED : New DHCP lease time amount should be entered successfully in text box.");
	    LOGGER.info("**********************************************************************************");
	    status = LanSideBasePage.selectAndValidateValueFromDropDown(
		    BroadBandWebGuiElements.XPATH_FOR_DHCP_LEASE_TIME_MEASURE,
		    BroadBandTestConstants.DHCP_DROP_DOWN_MINUTES);
	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : DHCP lease time amount entered successfully in text box.");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S17";
	    errorMessage = "Failed to verify new DHCP Lease time via webpa.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Verify new DHCP Lease time reflected via webpa or not.");
	    LOGGER.info(
		    "STEP 17: ACTION : Execute the webpa command : curl -X GET -H \"Authorization: Bearer <SAT token>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i <WEBPA URL>/v2/device/mac:<CM_MAC>/config?names=Device.DHCPv4.Server.Pool.1.LeaseTime");
	    LOGGER.info("STEP 17: EXPECTED : New DHCP Lease time must reflect via webpa.");
	    LOGGER.info("**********************************************************************************");
	    /** Clicking on 'Save Settings' button */
	    LanSideBasePage.click(By.xpath(BroadBandWebGuiElements.XPATH_FOR_SAVE_BUTTON_DHCP_CONFIGURATIONS));
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    String newDhcpLeaseTime = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME);
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    BroadBandTestConstants.STRING_360, newDhcpLeaseTime);
	    long newLeaseTimeStartTime = System.currentTimeMillis();
	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : New DHCP Lease time reflected successfully.");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S18";
	    errorMessage = "Unable to disconnect connected client.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION : Verify Connected client is disconnected.");
	    LOGGER.info("STEP 18: ACTION : Disconnect connected client from SSID.");
	    LOGGER.info("STEP 18: EXPECTED : Connected client should be disconnected successfully.");
	    LOGGER.info("**********************************************************************************");
	    BroadBandResultObject disconnectionStatus = BroadBandConnectedClientUtils
		    .disconnectCnnClientFromSsid(tapEnv, device, deviceConnectedWith2GhzWifi);
	    status = disconnectionStatus.isStatus();
	    errorMessage = disconnectionStatus.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Client disconnected successfully.");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S19";
	    errorMessage = "Unable to connect same client to 2.4GHz Wi-Fi.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 19 DESCRIPTION : Verify same Client get connected to 2.4GHz Wi-Fi and Local IP Page reloaded.");
	    LOGGER.info(
		    "STEP 19: ACTION : Connect same client to 2.4GHz Wi-Fi based on MAC Address and reload Local IP Page.");
	    LOGGER.info(
		    "STEP 19 EXPECTED : Same Client should be Connected to 2.4GHz Wi-Fi and Local IP Page should be reloaded.");
	    LOGGER.info("**********************************************************************************");
	    reConnectionStatus = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
		    deviceConnectedWith2GhzWifi, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    status = reConnectionStatus.isStatus();
	    errorMessage = reConnectionStatus.getErrorMessage();
	    LOGGER.info("Waiting for 30 Seconds after reconnecting to Wi-Fi.");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    if (status) {
		try {
		    LOGGER.info("Refreshing and Validating local IP page title.");
		    long startTime = System.currentTimeMillis();
		    do {
			/** Refreshing Local IP Page */
			LOGGER.info("IP page Refresh");
			lanSidePageNavigation.refresh();
			status = BroadBandWebUiUtils.validatePageLaunchedStatusWithPageTitle(webDriver,
				BroadbandPropertyFileHandler.getPageTitleForLocalIpNetwork());

		    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
			    && !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				    BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS));

		} catch (Exception e) {
		    errorMessage = errorMessage + e.getMessage();
		    LOGGER.error(errorMessage);
		}
	    }

	    LOGGER.info("Status is: " + status);
	    if (status) {
		LOGGER.info(
			"STEP 19: ACTUAL : Same Client Connected to 2.4GHz Wi-Fi successfully and Local IP page reloaded successfully.");
	    } else {
		LOGGER.error("STEP 19 ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S20";
	    errorMessage = "Unable to verify client IP when lease time is active.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 20: DESCRIPTION : Verify Client IP when lease time is active.");
	    LOGGER.info(
		    "STEP 20: ACTION : Get the device IPv4 address using below commandLinux : ifconfig wlan0\\eth0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Wireless\\Ethernet LAN adapter Wi-Fi\" |grep -i \"IPv4 Address\"");
	    LOGGER.info(
		    "STEP 20: EXPECTED : Client Ip should be assigned to client and within DHCP Range when lease time is active.");
	    LOGGER.info("**********************************************************************************");
	    if (((System.currentTimeMillis() - newLeaseTimeStartTime) / 1000) < 360) {
		status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv,
			device, deviceConnectedWith2GhzWifi);
	    } else {
		errorMessage = "Lease Time is already Expired.";
	    }
	    if (status) {
		LOGGER.info("STEP 20: ACTUAL : Client IP verified when lease time is active.");
	    } else {
		LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S21";
	    errorMessage = "Unable to disconnect connected client.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 21: DESCRIPTION : Verify Connected client is disconnected.");
	    LOGGER.info("STEP 21: ACTION : Disconnect connected client from SSID.");
	    LOGGER.info("STEP 21: EXPECTED : Connected client should be disconnected successfully.");
	    LOGGER.info("**********************************************************************************");
	    while (true) {
		if (((System.currentTimeMillis() - newLeaseTimeStartTime) / 1000) > 360) {
		    disconnectionStatus = BroadBandConnectedClientUtils.disconnectCnnClientFromSsid(tapEnv, device,
			    deviceConnectedWith2GhzWifi);
		    status = disconnectionStatus.isStatus();
		    errorMessage = disconnectionStatus.getErrorMessage();
		    break;
		}
	    }
	    if (status) {
		LOGGER.info("STEP 21: ACTUAL : Client disconnected successfully.");
	    } else {
		LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S22";
	    errorMessage = "Unable to connect same client to 2.4GHz Wi-Fi.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 22: DESCRIPTION : Verify same Client get connected to 2.4GHz Wi-Fi.");
	    LOGGER.info("STEP 22: ACTION : Connect same client to 2.4GHz Wi-Fi based on MAC Address.");
	    LOGGER.info("STEP 22: EXPECTED : Same Client should be Connected to 2.4GHz Wi-Fi.");
	    LOGGER.info("**********************************************************************************");
	    reConnectionStatus = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
		    deviceConnectedWith2GhzWifi, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    status = reConnectionStatus.isStatus();
	    errorMessage = reConnectionStatus.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 22: ACTUAL : Same Client Connected to 2.4GHz Wi-Fi successfully.");
	    } else {
		LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S23";
	    errorMessage = "Unable to verify reuse ip after disconnecting & disconnecting.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 23: DESCRIPTION : Verify client reuse same IP address when disconnected & reconnected ");
	    LOGGER.info(
		    "STEP 23: ACTION : Get the device IPv4 address using below commandLinux : ifconfig wlan0\\eth0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Wireless\\Ethernet LAN adapter Wi-Fi\" |grep -i \"IPv4 Address\"");
	    LOGGER.info("STEP 23: EXPECTED : Device should reuse same IP address after disconnected & reconnected ");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Waiting for 1 Minute for regaining IPv4 Address.");
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
		    deviceConnectedWith2GhzWifi);
	    if (status) {
		LOGGER.info("STEP 23: ACTUAL : Reuse ip after disconnecting & disconnecting successfully.");
	    } else {
		LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    errorMessage = "Failed to set the default DHCP IPv4 values.";
	    status = false;
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION : Verify DHCP IPv4 values are set to default.");
	    LOGGER.info("POST-CONDITION 1: ACTION : Set the default DHCP IPv4 values using WEBPA.");
	    LOGGER.info("POST-CONDITION 1: EXPECTED : Must set the default DHCP IPv4 values.");
	    defaultDhcpIpv4ValuesMap = BroadBandCommonUtils.addDefaultDhcpValuesinMap(device);
	    status = BroadBandWebGuiAdminPageTest.executePostConditionToSetTheDefaultDchpIpv4Values(device,
		    defaultDhcpIpv4ValuesMap);
	    if (status) {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : Successfully set the default DHCP IPv4 values.");
	    } else {
		LOGGER.error("POST-CONDITION 1 : ACTUAL : " + errorMessage);
	    }

	    if (isBrowserOpen) {
		LOGGER.info("POST-CONDITION 2: DESCRIPTION : Verify browser is closed in connected client.");
		LOGGER.info("POST-CONDITION 2: EXPECTED : Browser should be closed successfully in Connected Client.");
		try {
		    LanWebGuiLoginPage.closeBrowser();
		    LOGGER.info("POST-CONDITION 2: ACTUAL : Browser closed successfully in Connected Client.");
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred while closing the browser, unable to close browser.");
		}
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-7034");
    }

    /**
     * Verify IP ping and traceroutE diagnostics for IPv4 & IPv6 Address from Gateway Diagnostics page
     * <ol>
     * <li>Verify the wireless client is connected and have private 2.4Ghz wifi Capability.</li>
     * <li>Verify the Connected client has got the IPv4 Address between DHCP Range.</li>
     * <li>Verify the Connected client has got the valid IPv6 Address.</li>
     * <li>Verify the Internet is accessible in the Connected client.</li>
     * <li>Launch Broad band WebUI login page and verify login status.</li>
     * <li>Verify Navigation to troubleshooting diagnostic tool page.</li>
     * <li>Verify valid IPv4 address entered in IPv4 address field.</li>
     * <li>Verify Ping Test and Connectivity status.</li>
     * <li>Verify valid IPv4 address entered in IPv4 address field in TraceRoute Results section.</li>
     * <li>Verify traceroute status for entered Valid Ipv4 Address.</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @author Prashant Mishra
     * 
     * @Refactor Sruthi Santhosh
     * 
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBUI-7040")
    public void verfifyPingConnectivityAndTraceRouteFromUserAdminPageForIpv4Address(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBUI-740";
	String stepNum = "";
	String errorMessage = "";
	String ipAddressToCheck = "";
	String traceOutput = "";
	boolean status = false;
	boolean isBrowserOpen = false;
	Dut deviceConnectedWith2GhzWifi = null;
	WebDriver webDriver = null;
	List<String> valueToBeEnteredInIpBox = new ArrayList<>();
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-7040");
	LOGGER.info(
		"TEST DESCRIPTION: Verify IP ping and tracerout diagnostics for IPv4 & IPv6 Address from Gateway Diagnostics page");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the wireless client is connected and have private 2.4Ghz wifi Capability.");
	LOGGER.info("2. Verify the Connected client has got the IPv4 Address between DHCP Range.");
	LOGGER.info("3. Verify the Connected client has got the valid IPv6 Address.");
	LOGGER.info("4. Verify the internet is accessible in the Connected client.");
	LOGGER.info("5. Launch Broad band WebUI login page and verify login status.");
	LOGGER.info("6. Verify Navigation to Diagnostic tools page.");
	LOGGER.info("7. Verify valid IPv4 address entered in IPv4 address field.");
	LOGGER.info("8. Verify Ping Test and Connectivity status.");
	LOGGER.info("9. Verify valid IPv4 address entered in IPv4 address field in TraceRoute Results section.");
	LOGGER.info("10. Verify traceroute status for entered Valid Ipv4 Address.");

	LOGGER.info("#######################################################################################");

	try {
	    stepNum = "S1";
	    errorMessage = "Failed to obtain a 2.4GHz WiFi client associated with the Gateway.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify the wireless client is connected and have private 2.4Ghz wifi Capability.");
	    LOGGER.info(
		    "STEP 1: ACTION : Connect to 2.4GHz Wi-Fi using below commands Linux :dmcli dev wifi connect <ssid> password <passwd>; Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP 1: EXPECTED : Device should be connected with 2.4GHz Wi-Fi Network.");
	    LOGGER.info("**********************************************************************************");
	    deviceConnectedWith2GhzWifi = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    status = null != deviceConnectedWith2GhzWifi;
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Obtained a WiFi 2.4GHz client assosiated with the gateway successfully.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * Checking Connected Client IPv4, IPv6 Address and Internet Connectivity
	     */
	    /** Step 2 to Step 4 */
	    BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
		    deviceConnectedWith2GhzWifi, tapEnv, BroadBandTestConstants.CONSTANT_2, testCaseId);

	    stepNum = "S5";
	    errorMessage = "Unable to launch Admin GUI page.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Launch Broad band WebUI login page and verify login status.");
	    LOGGER.info(
		    "STEP 5: ACTION : Launch the below URL format in browser :https://10.0.0.1/ for Residential device and https://10.1.10.1/ for Business class devicesLOGIN CREDENTIALS :  username: <admin> Password: <paswrd>");
	    LOGGER.info("STEP 5: EXPECTED : LAN Gui admin page should be launched and login should be successful.");
	    LOGGER.info("**********************************************************************************");
	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnectedWith2GhzWifi);
	    isBrowserOpen = status;
	    webDriver = LanWebGuiLoginPage.getDriver();
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Launching Broad band LanUI login page and login status is successful.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S6";
	    errorMessage = "Failed to navigate to the troubleshooting diagnostic page.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify Navigation to the troubleshooting page.");
	    LOGGER.info(
		    "STEP 6: ACTION : Navigate to the troubleshooting diagnostic page and verify navigation status");
	    LOGGER.info("STEP 6: EXPECTED : Troubleshooting diagnostic page navigation must be successful");
	    LOGGER.info("**********************************************************************************");
	    LanSidePageNavigation lanSidePageNavigation = new LanSidePageNavigation(webDriver);
	    status = lanSidePageNavigation.navigateToTroubleShootingNwDiagToolsPage(device, tapEnv, webDriver);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Navigated successfully to troubleshooting diagnostic page.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S7";
	    errorMessage = "Failed to entered ipv4 address in ipv4 field.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify valid IPv4 address entered in IPv4 address field.");
	    LOGGER.info(
		    "STEP 7: ACTION : Enter 'facebook.com' Ip address in IPV4 address field and verify entered values.");
	    LOGGER.info("STEP 7: EXPECTED : Ipv4 address should be entered successfully in Text boxes.");
	    LOGGER.info("**********************************************************************************");
	    String nslookupResponse = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_NSLOOKUP_WITH_PATH,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK));
	    if (CommonMethods.isNotNull(nslookupResponse)) {
		List<String> ipAddressList = BroadBandCommonUtils.patternFinderForMultipleMatches(nslookupResponse,
			BroadBandTestConstants.PATTERN_TO_RETRIVE_IP_ADDRESS_FROM_NSLOOKUP_RESPONSE,
			BroadBandTestConstants.CONSTANT_1);
		if (ipAddressList != null && ipAddressList.size() <= BroadBandTestConstants.CONSTANT_2) {
		    ipAddressToCheck = ipAddressList.get(BroadBandTestConstants.CONSTANT_1);
		}
	    }
	    /** Splitting IP Address in List to Enter in Boxes. */
	    valueToBeEnteredInIpBox = addIpElementInList(device, BroadBandTestConstants.BOOLEAN_VALUE_FALSE,
		    ipAddressToCheck);
	    status = lanSidePageNavigation.enterIpv4AddressInTextBoxesWithBoxesXpath(tapEnv, valueToBeEnteredInIpBox,
		    BroadBandWebGuiElements.XPATH_IPV4_BOXES_PING_CONNECTIVITY);
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Ipv4 values entered successfully in Ping Connectivity section.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S8";
	    errorMessage = "Failed to get the connectivity status for valid ipv4 address.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify Ping Test and Connectivity status.");
	    LOGGER.info("STEP 8: ACTION : Click 'CHECK FOR IP ADDRESSES' icon under ipv4 address field.");
	    LOGGER.info("STEP 8: EXPECTED : It should display connecticity status as 'OK'.");
	    LOGGER.info("**********************************************************************************");
	    BroadBandDiagnosticToolsPage diagnosticPage = new BroadBandDiagnosticToolsPage(webDriver);
	    try {
		status = diagnosticPage.verifyConnectivityTestForIpv4Address(tapEnv,
			BroadBandTestConstants.PING_CONNECTIVITY_TEST_MESSAGE);
	    } catch (Exception exception) {
		errorMessage = "Exception occurred while checking ipv4 connectivity : " + exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully verified ipv4 Connectivity status for valid ipv4 address.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S9";
	    errorMessage = "Failed to set ipv4 address in ipv4 field  in TraceRoute Results.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify valid IPv4 address entered in IPv4 address field in TraceRoute Results section.");
	    LOGGER.info(
		    "STEP 9: ACTION : Enter 'facebook.com' Ip address in IPV4 address field in TraceRoute Results.");
	    LOGGER.info(
		    "STEP 9: EXPECTED : Ipv4 address should be entered successfully in text boxes in TraceRoute Results section.");
	    LOGGER.info("**********************************************************************************");
	    status = lanSidePageNavigation.enterIpv4AddressInTextBoxesWithBoxesXpath(tapEnv, valueToBeEnteredInIpBox,
		    BroadBandWebGuiElements.XPATH_IPV4_BOXES_TRACEROUTE);
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Ipv4 address entered successfully in boxes.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S10";
	    errorMessage = "Unable to verify traceroute status.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify traceroute status for entered Valid Ipv4 Address.");
	    LOGGER.info("STEP 10: ACTION : Check for Status Ok and success message as 200.");
	    LOGGER.info("STEP 10: EXPECTED : 'Status: Complete !' keyword should be present in traceroute result.");
	    LOGGER.info("**********************************************************************************");
	    /** Clicking button for getting TraceRoute Output */
	    LanSideBasePage.click(By.xpath(BroadBandWebGuiElements.XPATH_BUTTON_TRACEROUTE));
	    /**
	     * Verifying Success message in output after every 10 seconds for duration of 3 minutes
	     */
	    long startTime = System.currentTimeMillis();
	    while ((System.currentTimeMillis() - startTime) / 1000 < 180) {
		tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
		traceOutput = LanSideBasePage.getText(By.xpath(BroadBandWebGuiElements.XPATH_TRACE_OUTPUT_MESSAGE));
		if (CommonMethods.isNotNull(traceOutput)
			&& traceOutput.contains(BroadBandTestConstants.TRACE_ROUTE_SUCCESS_MESSAGE)) {
		    status = BroadBandTestConstants.BOOLEAN_VALUE_TRUE;
		    break;
		}
	    }
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Traceout Result verified successfully.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    if (isBrowserOpen) {
		LOGGER.info("POST-CONDITION 1: DESCRIPTION : Verify the Browser is closed in connected client.");
		LOGGER.info("POST-CONDITION 1: ACTION : Close Browser in connected client.");
		LOGGER.info("POST-CONDITION 1: EXPECTED : Browser should be closed successfully in Connected Client.");
		try {
		    LanWebGuiLoginPage.closeBrowser();
		    LOGGER.info("POST-CONDITION 1: ACTUAL : Browser closed successfully in Connected Client.");
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred while closing the browser, unable to close browser.");
		}
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
    }

    /**
     * 
     * @param device
     *            instance of {@link Dut}
     * @param isGateWayIP
     *            Indicates whether IP to be split is GateWay IP or not
     * @param ipAddress
     *            It is not null if its connected Client IP address
     * @return valuesToBeEntered List contains all IP Address elements
     * 
     * @Refactor Sruthi Santhosh
     */
    public static List<String> addIpElementInList(Dut device, boolean isGateWayIP, String ipAddress) {
	List<String> valuesToBeEntered = new ArrayList<>();
	String[] hostValueToBeAdded;
	if (isGateWayIP) {
	    ipAddress = DeviceModeHandler.isBusinessClassDevice(device)
		    ? BroadBandTestConstants.STRING_BUSINESS_CLASS_GATEWAYIP
		    : BroadBandTestConstants.STRING_RESIDENTIAL_CLASS_GATEWAYIP;
	}
	if (CommonMethods.isNotNull(ipAddress)) {
	    hostValueToBeAdded = ipAddress.split("\\.");
	    for (int counter = 0; counter < hostValueToBeAdded.length; counter++) {
		valuesToBeEntered.add(hostValueToBeAdded[counter]);
	    }
	}
	return valuesToBeEntered;
    }

    /**
     * Test method to login into the Admin page in connected client setup
     * 
     * @param device
     *            {@link Dut}
     * @param deviceConnected
     *            instance of connected device
     * @param testCaseId
     *            Test case Id
     * @param driver
     *            {@link WebDriver}
     * @param stepNumber
     *            Test step number
     * @refactor Rakesh C N
     */
    public static void executeTestStepsToLoginAdminPage(Dut device, Dut deviceConnected, String testCaseId,
	    WebDriver driver, int stepNumber) {
	String errorMessage = null;
	boolean status = false;
	String stepNum = null;

	/**
	 * Step : VERIFY LOGIN INTO THE LAN GUI ADIMN PAGE BY USING VALID USERID AND VALID PASSWORD
	 */
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STEP " + stepNumber
		+ " : DESCRIPTION : VERIFY THE GATEWAY ADMIN PAGE IS ACCESSIBLE IN CONNECTED CLIENT AND CAN BE LOGGED IN USING ADMIN/****** CREDENTIAL FOR RESIDENTIAL OR CUSADMIN/****** CREDENTIAL FOR COMMERCIAL DEVICES");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : LAUNCH THE GATEWAY ADMIN GUI IN BROWSER URL : https://10.0.0.1 or https://10.1.10.1 , ONCE THE PAGE IS LOADED ,USE USERNAME AND PASSWORD AS ADMIN/****** FOR RESIDENTIAL OR CUSADMIN/****** FOR COMMERCIAL DEVICES TO LOGIN");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : GATEWAY ADMIN PAGE SHOULD BE ACCESSIBLE FROM CLIENT AND CAN BE ABLE TO LOGIN USING ADMIN/****** CREDENTIAL FOR RESIDENTIAL OR CUSADMIN/****** CREDENTIAL FOR COMMERCIAL DEVICES");
	LOGGER.info("#######################################################################################");
	errorMessage = "UNABLE TO LOGIN GATEWAY ADMIN PAGE IN CONNECTED CLIENT";
	try {
	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnected);
	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error("Exception occurred during Gateway Admin Page login : " + errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : LAN GUI ADMIN LOGIN SUCCESSFUL");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("#######################################################################################");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);
    }

    /**
     * Test method to Navigate into the Public Network page in connected client setup
     * 
     * @param device
     *            {@link Dut}
     * @param lanSidePageNavigation
     *            {@link LanSidePageNavigation}
     * @param deviceConnected
     *            instance of connected device
     * @param testCaseId
     *            Test case Id
     * @param driver
     *            {@link WebDriver}
     * @param stepNumber
     *            Test step number
     * @refactor Rakesh C N
     */
    public static void executeTestStepsToLanuchPublicWiFiPage(Dut device, LanSidePageNavigation lanSidePageNavigation,
	    String testCaseId, WebDriver driver, int stepNumber) {
	String errorMessage = null;
	boolean status = false;
	String stepNum = null;
	boolean isBusinessClsDevice = DeviceModeHandler.isBusinessClassDevice(device);
	/**
	 * STEP : NAVIGATE TO THE GATEWAY > CONNECTION > PUBLIC NETWORK PAGE FOR RESIDENTIAL OR GATEWAY > CONNECTION >
	 * PUBLIC NETWORK PAGE FOR COMMERCIAL DEVICES AND VERIFY NAVIGATION STATUS
	 */
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STEP " + stepNumber
		+ " : DESCRIPTION : NAVIGATE TO THE GATEWAY > CONNECTION > PUBLIC NETWORK PAGE FOR RESIDENTIAL OR GATEWAY > CONNECTION > PUBLIC NETWORK PAGE FOR COMMERCIAL DEVICES AND VERIFY NAVIGATION STATUS");
	LOGGER.info("STEP " + stepNumber
		+ " : ACTION : CLICK ON GATEWAY > CONNECTION > PUBLIC NETWORK(FOR RESIDENTIAL DEVICE) OR GATEWAY > CONNECTION > PUBLIC NETWORK(FOR COMMERCIAL DEVICE)");
	LOGGER.info("STEP " + stepNumber
		+ " : EXPECTED : NAVIGATION SHOULD BE SUCCESSFUL AND IT SHOULD DISPLAY THE GATEWAY > CONNECTION > PUBLIC NETWORK PAGE (FOR RESIDENTIAL DEVICE) OR GATEWAY > CONNECTION > PUBLIC NETWORK PAGE (FOR COMMERCIAL DEVICE)");
	LOGGER.info("#######################################################################################");
	errorMessage = "UNABLE TO VERIFY NAVIGATION STATUS ON GATEWAY > CONNECTION > PUBLIC NETWORK PAGE(FOR RESIDENTIAL DEVICE) OR GATEWAY > CONNECTION > PUBLIC NETWORK(FOR COMMERCIAL DEVICE)PAGE";
	status = lanSidePageNavigation.navigateToPartnerNetworkPage(device, tapEnv, driver, isBusinessClsDevice);
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > CONNECTION > PUBLIC NETWORK PAGE (FOR RESIDENTIAL DEVICE) OR GATEWAY > CONNECTION > PUBLIC NETWORK PAGE (FOR COMMERCIAL DEVICE) ");
	} else {
	    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("#######################################################################################");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);
    }

    /**
     * Test method to validate the Upstream and Downstream values In Public Wifi page
     * 
     * @param device
     *            {@link Dut}
     * @param testCaseId
     *            Test case Id
     * @param driver
     *            {@link WebDriver}
     * @param stepNumber
     *            Test step number
     * @param dsTblIndex
     *            Index for Down stream table
     * @param usTblIndex
     *            Index for Up stream table
     * @refactor Rakesh C N
     */
    public static void validateUpstreamDownsteramValuesInPublicPage(Dut device, String testCaseId, WebDriver driver,
	    int stepNumber, String dsTblIndex, String usTblIndex) {
	boolean status = false;
	String errorMessage = null;
	String responseFromUi = null;
	String webPaResponse = null;
	String stepNum = null;

	/**
	 * SETP : GET THE DOWNSTREAM VALUES IN PUBLIC WIFI NETWORK PAGE
	 */
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber + " : DESCRIPTION : GET THE DOWNSTREAM VALUES IN PUBLIC WIFI NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber + " : ACTION : READ THE DOWNSTREAM VALUES IN PUBLIC WIFI NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : EXPECTED : MUST RETURN THE DOWNSTREAM VALUES FROM PUBLIC WIFI NETWORK PAGE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "UNABLE TO GET THE DOWNSTREAM VALUES IN PUBLIC WIFI NETWORK PAGE";
	try {
	    responseFromUi = driver
		    .findElement(By.xpath(BroadBandWebGuiElements.XPATH_FOR_UPDOWNSTREAM_TABLE
			    .replace(BroadBandTestConstants.STRING_REPLACE, dsTblIndex)))
		    .getText().replaceAll(BroadBandTestConstants.PATTERN_MATCHER_FOR_MULTIPLE_SPACES,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	} catch (NoSuchElementException noSuchElementException) {
	    // Log & Suppress the Exception
	    LOGGER.error(noSuchElementException.getMessage());
	}
	LOGGER.info("RESPONSE FORM DOWN STREAM TABLE :" + responseFromUi);
	status = CommonMethods.isNotNull(responseFromUi)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_LOCK_STATUS)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_FREQUENCY)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_SNR)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_POWER_LEVEL)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_MODULATION);
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY RETRIEVED THE DOWNSTREAM VALUES IN PUBLIC WIFI NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	/**
	 * SETP : VERIFY THE LOCK STATUS VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE LOCK STATUS VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE LOCK STATUS VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE DOWNSTREAM TABLE AND COMPARE THE LOCK STATUS VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE LOCK STATUS VALUE FROM DOWNSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE LOCK STATUS VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_LOCKSTATUS
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	status = CommonMethods.isNotNull(webPaResponse)
		&& (CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse)
			|| responseFromUi.contains(webPaResponse));
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE LOCK STATUS VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : VERIFY THE FREQUENCY VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE FREQUENCY VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE FREQUENCY VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE DOWNSTREAM TABLE AND COMPARE THE FREQUENCY VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE FREQUENCY VALUE FROM DOWNSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE FREQUENCY VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_FREQUENCY
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	status = CommonMethods.isNotNull(webPaResponse)
		&& (CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse)
			|| responseFromUi.contains(webPaResponse));
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE FREQUENCY VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : VERIFY THE SNR VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE SNR VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE SNR VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE DOWNSTREAM TABLE AND COMPARE THE SNR VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE SNR VALUE FROM DOWNSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE SNR VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_SNRLEVEL
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	status = CommonMethods.isNotNull(webPaResponse)
		&& (CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse)
			|| responseFromUi.contains(webPaResponse));
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE SNR VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : VERIFY THE POWER LEVEL VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE POWER LEVEL VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE POWER LEVEL VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE DOWNSTREAM TABLE AND COMPARE THE POWER LEVEL VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE POWER LEVEL VALUE FROM DOWNSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE POWER LEVEL VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_POWERLEVEL
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	status = CommonMethods.isNotNull(webPaResponse)
		&& (CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse)
			|| responseFromUi.contains(webPaResponse));
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE POWER LEVEL VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : VERIFY THE MODULATION VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE MODULATION VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE MODULATION VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE DOWNSTREAM TABLE AND COMPARE THE MODULATION VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE MODULATION VALUE FROM DOWNSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE MODULATION VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_DOWNSTREAM_CHANNEL_MODULATION
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	status = CommonMethods.isNotNull(webPaResponse)
		&& (CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse)
			|| responseFromUi.contains(webPaResponse));
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE MODULATION VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : GET THE UPSTERAM VALUES IN PUBLIC WIFI NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber + " : DESCRIPTION : GET THE UPSTERAM VALUES IN PUBLIC WIFI NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber + " : ACTION : READ THE UPSTERAM VALUES IN PUBLIC WIFI NETWORK PAGE");
	LOGGER.info(
		"STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE UPSTERAM VALUES FROM PUBLIC WIFI NETWORK PAGE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "UNABLE TO GET THE UPSTERAM VALUES IN PUBLIC WIFI NETWORK PAGE";
	try {
	    responseFromUi = driver
		    .findElement(By.xpath(BroadBandWebGuiElements.XPATH_FOR_UPDOWNSTREAM_TABLE
			    .replace(BroadBandTestConstants.STRING_REPLACE, usTblIndex)))
		    .getText().replaceAll(BroadBandTestConstants.PATTERN_MATCHER_FOR_MULTIPLE_SPACES,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	} catch (NoSuchElementException noSuchElementException) {
	    // Log & Suppress the Exception
	    LOGGER.error(noSuchElementException.getMessage());
	}
	LOGGER.info("RESPONSE FORM DOWN STREAM TABLE :" + responseFromUi);
	status = CommonMethods.isNotNull(responseFromUi)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_LOCK_STATUS)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_FREQUENCY)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_SYMBOL_RATE)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_POWER_LEVEL)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_MODULATION)
		&& CommonUtils.patternSearchFromTargetString(responseFromUi,
			BroadBandWebGuiTestConstant.UP_AND_DOWN_STREAM_PARAM_CHANNEL_TYPE);
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY RETRIEVED THE UPSTERAM VALUES IN PUBLIC WIFI NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	/**
	 * SETP : VERIFY THE LOCK STATUS VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE LOCK STATUS VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE LOCK STATUS VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE UPSTREAM TABLE AND COMPARE THE LOCK STATUS VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE LOCK STATUS VALUE FROM UPSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE LOCK STATUS VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_UPSTREAM_CHANNEL_LOCKSTATUS
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	status = CommonMethods.isNotNull(webPaResponse)
		&& (CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse)
			|| responseFromUi.contains(webPaResponse));
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE LOCK STATUS VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : VERIFY THE FREQUENCY VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE FREQUENCY VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE FREQUENCY VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE UPSTREAM TABLE AND COMPARE THE FREQUENCY VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE FREQUENCY VALUE FROM UPSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE FREQUENCY VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_UPSTREAM_CHANNEL_FREQUENCY
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	String responseUi = responseFromUi.replaceAll("\\s", "");
	String response = webPaResponse.replaceAll("\\s", "");
	status = CommonMethods.isNotNull(response)
		&& (CommonUtils.patternSearchFromTargetString(responseUi, response) || responseUi.contains(response));

	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE FREQUENCY VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : VERIFY THE SYMBOL RATE VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE SYMBOL RATE VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE SYMBOL RATE VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE UPSTREAM TABLE AND COMPARE THE SNR VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE SYMBOL RATE VALUE FROM UPSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE SYMBOL RATE VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_UPSTREAM_CHANNEL_SYMBOLRATE
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	status = CommonMethods.isNotNull(webPaResponse)
		&& (CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse)
			|| responseFromUi.contains(webPaResponse));
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE SYMBOL RATE VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : VERIFY THE POWER LEVEL VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE POWER LEVEL VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE POWER LEVEL VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE UPSTREAM TABLE AND COMPARE THE POWER LEVEL VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE POWER LEVEL VALUE FROM UPSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE POWER LEVEL VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_UPSTREAM_CHANNEL_POWERLEVEL
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	status = CommonMethods.isNotNull(webPaResponse)
		&& (CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse)
			|| responseFromUi.contains(webPaResponse));
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE POWER LEVEL VALUE FROM DOWNSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : VERIFY THE MODULATION VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE MODULATION VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE MODULATION VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE UPSTREAM TABLE AND COMPARE THE MODULATION VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE MODULATION VALUE FROM UPSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE MODULATION VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_UPSTREAM_CHANNEL_MODULATION
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	status = CommonMethods.isNotNull(webPaResponse)
		&& (CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse)
			|| responseFromUi.contains(webPaResponse));
	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE MODULATION VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	/**
	 * SETP : VERIFY THE CHANNEL TYPE VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE
	 */
	stepNumber++;
	stepNum = "S" + stepNumber;
	status = false;
	errorMessage = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP : " + stepNumber
		+ " : DESCRIPTION : VERIFY THE CHANNEL TYPE VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	LOGGER.info("STEP : " + stepNumber
		+ " : ACTION : CHECK THE CHANNEL TYPE VALUE IS AVAILABLE IN PUBLIC NETWORK PAGE UPSTREAM TABLE AND COMPARE THE CHANNEL TYPE VALUE WITH WEBPA");
	LOGGER.info("STEP : " + stepNumber + " : EXPECTED : MUST RETURN THE CHANNEL TYPE VALUE FROM UPSTREAM TABLE");
	LOGGER.info("**********************************************************************************");
	errorMessage = "FAILED TO GET THE CHANNEL TYPE VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE";
	webPaResponse = tapEnv.executeWebPaCommand(device,
		BroadBandWebPaConstants.WEBPA_PARAM_UPSTREAM_CHANNEL_CHANNELTYPE
			.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	if (CommonMethods.isNotNull(webPaResponse)) {
	    webPaResponse = webPaResponse.replace("(", "\\(");
	    webPaResponse = webPaResponse.replace(")", "\\)");
	    status = CommonUtils.patternSearchFromTargetString(responseFromUi, webPaResponse);
	}

	if (status) {
	    LOGGER.info("STEP : " + stepNumber
		    + " : ACTUAL : SUCCESSFULLY VERIFIED THE CHANNEL TYPE VALUE FROM UPSTREAM TABLE IN PUBLIC NETWORK PAGE");
	} else {
	    LOGGER.error("STEP : " + stepNumber + " : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
    }

    /**
     *
     * Test Case : Verify channel type, Upstream and Downstream information availability in admin login Public network
     * page for 5 Ghz
     *
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION 1 : Connect the client setup to 5 GHz SSID and verify connection status</li>
     * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected with 5 GHz SSID</li>
     * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected with 5 GHz SSID</li>
     * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface</li>
     * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface</li>
     * <li>Step 1 : Verify login into the LAN GUI Admin page by using valid userid and valid password</li>
     * <li>Step 2 : Navigate to the Gateway>Connection>PUBLIC Network page and verify Navigation Status</li>
     * <li>Step 3 : Get the the downstream values in Public wifi network page</li>
     * <li>Step 4 : Verify the Lock Status value from downstream table in Public Network page and compare the Lock
     * Status value with webpa response *</li>
     * <li>Step 5 : Verify the Frequency value from downstream table in Public Network page and compare the Frequency
     * value with webpa response</li>
     * <li>Step 6 : Verify the SNR value from downstream table in Public Network page and compare the SNR value with
     * webpa response</li>
     * <li>Step 7 : Verify the Power Level value from downstream table in Public Network page compare the Power Level
     * value with webpa response</li>
     * <li>Step 8 : Verify the Modulation value from downstream table in Public Network page compare the Modulation
     * value with webpa response</li>
     * <li>Step 9 : Get the the upstream values in Public wifi network page</li>
     * <li>Step 10 : Verify the Lock Status value from upstream table in Public Network page compare the Lock Status
     * value with webpa response</li>
     * <li>Step 11 : Verify the Frequency value from upstream table in Public Network page compare the Frequency value
     * with webpa response</li>
     * <li>Step 12 : Verify the Symbol Rate from upstream table in Public Network page compare the Symbol Rate value
     * with webpa response</li>
     * <li>Step 13 : Verify the Power Level value from upstream table in Public Network page compare the Power Level
     * value with webpa response</li>
     * <li>Step 14 : Verify the Modulation value from upstream table in Public Network page compare the Modulation value
     * with webpa response</li>
     * <li>Step 15 : Verify the Channel Type value from upstream table in Public Network page compare the Channel Type
     * value with webpa response</li>
     * <li>POST-CONDITION 1 : Close the Browser LAN Client</li>
     * <li>POST-CONDITION 2 : Verify disconnecting the 5 GHz private wifi SSID</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @author Muthukumar
     * @refactor Rakesh C N
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true)
    @TestDetails(testUID = "TC-RDKB-WIFI-5GHZ-PUBLIC-PAGE-5001")
    public void testToVerifyTheUpstreamAndDownStreamValuesInAdminLoginFor5Ghz(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI-5GHZ-PUBLIC-PAGE-501";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	LanSidePageNavigation lanSidePageNavigation = null;
	Dut deviceConnectedWith5Ghz = null;
	stepNumber = 1;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-5GHZ-PUBLIC-PAGE-5001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify channel type, Upstream and Downstream information availability in admin login Public network page for 5 Ghz");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1 : Connect  the  client setup to 5 GHz SSID and verify connection status");
	LOGGER.info("PRE-CONDITION 2 : Verify the correct IPv4 address for client connected with 5 GHz SSID");
	LOGGER.info("PRE-CONDITION 3 : Verify the correct IPv6 address for client connected with 5 GHz SSID");
	LOGGER.info(
		"PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
	LOGGER.info(
		"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface");
	LOGGER.info("Step 1. Verify login into the LAN GUI Admin page by using valid userid and valid password");
	LOGGER.info("Step 2. Navigate to the Gateway>Connection>PUBLIC Network page and verify Navigation Status");
	LOGGER.info("Step 3 : Get the the downstream values in Public wifi network page");
	LOGGER.info(
		"Step 4 : Verify the Lock Status value from downstream table in Public Network page and compare the Lock Status value with webpa response  * ");
	LOGGER.info(
		"Step 5 : Verify the Frequency value from downstream table in Public Network page and compare the Frequency value with webpa response");
	LOGGER.info(
		"Step 6 : Verify the SNR value from downstream table in Public Network page and compare the SNR value with webpa response");
	LOGGER.info(
		"Step 7 : Verify the Power Level value from downstream table in Public Network page compare the Power Level value with webpa response");
	LOGGER.info(
		"Step 8 : Verify the Modulation value from downstream table in Public Network page compare the Modulation value with webpa response");
	LOGGER.info("Step 9 : Get the the upstream values in Public wifi network page");
	LOGGER.info(
		"Step 10 : Verify the Lock Status value from upstream table in Public Network page compare the Lock Status value with webpa response");
	LOGGER.info(
		"Step 11 : Verify the Frequency value from upstream table in Public Network page compare the Frequency value with webpa response");
	LOGGER.info(
		"Step 12 : Verify the Symbol Rate from upstream table in Public Network page compare the Symbol Rate value with webpa response");
	LOGGER.info(
		"Step 13 : Verify the Power Level value from upstream table in Public Network page compare the Power Level value with webpa response");
	LOGGER.info(
		"Step 14 : Verify the Modulation value from upstream table in Public Network page compare the Modulation value with webpa response");
	LOGGER.info(
		"Step 15 : Verify the Channel Type value from upstream table in Public Network page compare the Channel Type value with webpa response");
	LOGGER.info("POST-CONDITION 1 : Close the Browser LAN Client");
	LOGGER.info("POST-CONDITION 2 : Verify disconnecting the 5 GHz private wifi SSID");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    deviceConnectedWith5Ghz = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
		    tapEnv, BroadBandTestConstants.BAND_5GHZ);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * Step 1 : VERIFY LOGIN INTO THE LAN GUI ADMIN PAGE BY USING VALID USERID AND VALID PASSWORD
	     */
	    executeTestStepsToLoginAdminPage(device, deviceConnectedWith5Ghz, testCaseId, driver, stepNumber);
	    isBrowserOpen = status;
	    driver = LanWebGuiLoginPage.getDriver();
	    LOGGER.info(" webDriver " + driver);
	    lanSidePageNavigation = new LanSidePageNavigation(driver);

	    /**
	     * STEP 2 : NAVIGATE TO THE GATEWAY > CONNECTION > PUBLIC NETWORK PAGE FOR RESIDENTIAL OR GATEWAY >
	     * CONNECTION > PUBLIC NETWORK PAGE FOR COMMERCIAL DEVICES AND VERIFY NAVIGATION STATUS
	     */
	    stepNumber++;
	    executeTestStepsToLanuchPublicWiFiPage(device, lanSidePageNavigation, testCaseId, driver, stepNumber);

	    /**
	     * SETP 3-15 : VLIDATING UPSTREAM AND DOWNSTREAM VALUES
	     */
	    stepNumber++;
	    validateUpstreamDownsteramValuesInPublicPage(device, testCaseId, driver, stepNumber,
		    BroadBandTestConstants.STRING_VALUE_FIVE, BroadBandTestConstants.STRING_VALUE_SIX);
	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING UPSTREAM AND DOWNSTREAM VALUES IN LAN ADMIN 5 GHZ"
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    /**
	     * Post-condition 1 : Close the LAN Side Browser
	     */
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
	    LOGGER.info("POST-CONDITION 1 : ACTION : CLOSE THE LAN SIDE BROWSER");
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
	    if (isBrowserOpen) {
		try {
		    LanSidePageNavigation.closeBrowser();
		    LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
		} catch (Exception exception) {
		    LOGGER.info(
			    "POST-CONDITION 1 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
		}
	    } else {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
	    }
	    /**
	     * Post-condition 2 : Disconnect the 5 GHz private wifi SSID
	     */
	    BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith5Ghz(device, tapEnv,
		    deviceConnectedWith5Ghz, BroadBandTestConstants.CONSTANT_2);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE:TC-RDKB-WIFI-5GHZ-PUBLIC-PAGE-5001");
    }

    /**
     * Method to verify setting WiFi client ip as static ip and verify internet connectivity
     * 
     * <ol>
     * <li>Verify connecting a WiFi client(2.4GHz or 5GHz) associated with the gateway.</li>
     * <li>Verify the correct Ipv4 address for wifi Client.</li>
     * <li>Verify the correct Ipv6 address for wifi Client.</li>
     * <li>Verify the internet connectivity in the connected wifi client.</li>
     * <li>Verify retrieving the host name & Ipv4 address of wireless client.</li>
     * <li>Verify launching BroadBand Lan UI login page and verify and login status.</li>
     * <li>Verify navigating to connected device page.</li>
     * <li>Verify navigating to Connected Devices > Devices > Edit Device.</li>
     * <li>Verify adding client ip as static ip.</li>
     * <li>Verify the internet connectivity in the connected wifi client after setting client ip as static.</li>
     * <li>Verify rebooting the device.</li>
     * <li>Verify client ip is set as static ip or not.</li>
     * </ol>
     * 
     * @param device{@link
     *            Dut}
     * 
     * @author prashant.mishra12
     * @refactor Govardhan
     */
    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true)
    @TestDetails(testUID = "TC-RDKB-WEBUI-8029")
    public void testToValidateSettingWifiClientIpAsStatic(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBUI-029";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	Dut connectedClientDevice = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-8029");
	LOGGER.info("TEST DESCRIPTION: Verify setting Wifi Client ip as static ip and verify internet connectivity.");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify connecting a WiFi client(2.4GHz or 5GHz) associated with the gateway.");
	LOGGER.info("2. Verify the correct Ipv4 address for wifi Client.");
	LOGGER.info("3. Verify the correct Ipv6 address for wifi Client.");
	LOGGER.info("4. Verify the internet connectivity in the connected wifi client.");
	LOGGER.info("5. Verify retrieving the host name & Ipv4 address of wireless client.");
	LOGGER.info("6. Verify launching BroadBand Lan UI login page and verify and login status.");
	LOGGER.info("7. Verify navigating to connected device page.");
	LOGGER.info("8. Verify navigating to Connected Devices > Devices > Edit Device.");
	LOGGER.info("9. Verify adding client ip as static ip.");
	LOGGER.info(
		"10. Verify the internet connectivity in the connected wifi client after setting client ip as static.");
	LOGGER.info("11. Verify rebooting the device.");
	LOGGER.info("12. Verify client ip is set as static ip or not.");

	LOGGER.info("#######################################################################################");

	try {
	    stepNum = "s1";
	    errorMessage = "Unable to connect wifi client.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify connecting a WiFi client(2.4GHz or 5GHz) associated with the gateway.");
	    LOGGER.info("STEP 1: ACTION : Connect one of the associated wifi client to 2.4 or 5GHz wifi.");
	    LOGGER.info("STEP 1: EXPECTED : One of the associated wifi client should be connected successfully.");
	    LOGGER.info("**********************************************************************************");
	    try {
		connectedClientDevice = BroadBandConnectedClientUtils
			.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = null != connectedClientDevice;
	    if (status) {
		LOGGER.info("GOING TO WAIT FOR 1 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		LOGGER.info("STEP 1: ACTUAL : One of associated wifi client connected to wifi successfully.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    // step-2 to step-4
	    BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
		    connectedClientDevice, tapEnv, BroadBandTestConstants.CONSTANT_2, testCaseId);

	    /** Common method for verify setting client ip as static ip */
	    methodToVerifySettingClientIpAsStaticForAllClients(device, connectedClientDevice, tapEnv, testCaseId,
		    BroadBandTestConstants.INTEGER_VALUE_5, BroadBandTestConstants.CLIENT_TYPE_WIFI);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-8029");
    }
    
    /**
    *
    * Test Case : Verify whether the CM can act as a DHCPv4 server for 2.4GHz Wi-Fi Client Scenario.
    *
    * <p>
    * STEPS:
    * </p>
    * <ol>
    * <li>PRE-CONDITION 1 : Obtain the Wi-Fi client 2.4 GHz associated with the gateway.</li>
    * <li>PRE-CONDITION 2 : Verify the correct IPV4 address for the Wi-Fi client.</li>
    * <li>PRE-CONDITION 3 : Verify the correct IPV6 address for the Wi-Fi client.</li>
    * <li>PRE-CONDITION 4 : Verify the Internet Connectivity in the Connected Wi-Fi client using IPV4 interface.</li>
    * <li>PRE-CONDITION 5 : Verify the Internet Connectivity in the Connected Wi-Fi client using IPV6 interface.</li>
    * <li>PRE-CONDITION 6 : Get Default DHCP beginning address ,DHCP ending address ,DHCP lease time Using Webpa</li>
    * <li>Step 1: Launch Broad band WebUI login page and verify login status</li>
    * <li>Step 2: Launch the Local IP Network page from AdminUI page</li>
    * <li>Step 3: Verify and Set the random DHCP IPV4 beginning address in Local IP Network Page.</li>
    * <li>Step 4: Verify and Set the random DHCP IPV4 ending address in Local IP Network Page.</li>
    * <li>Step 5: Verify the new DHCPv4 configuration setting are saved in the Local IP Network page.</li>
    * <li>Step 6: Change DHCP lease time to 2 minutes of Connected client 2 using SNMP
    * oid(1.3.6.1.4.1.1429.50.2.3.3.1.5.32)</li>
    * <li>Step 7: Verify and Retrieve the new DHCPv4 configuration for beginning and ending address is set using
    * WEBPA.</li>
    * <li>Step 8: Verify IPv4 is assigned on the Wi-Fi client</li>
    * <li>Step 9: Verify the Internet Connectivity in the connected Wi-Fi client using ipv4 interface.</li>
    * <li>Post-Condition : Close the Browser.</li>
    * <li>Post-Condition : Revert the DHCP configurations to default values.</li>
    * </ol>
    * 
    * @param device
    *            {@link Dut}
    * @author Joseph M
    */
   @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
   @TestDetails(testUID = "TC-RDKB-WEBUI-7006")

   public void setDhcpMinMaxRangeUsing2_4GhzWifiClient(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";

	Dut deviceConnectedWith2_4GhzWifi = null; // connected device to be verified
	String errorMessage = "unable to obtain 2.4Ghz client or Client configuration is invalid";
	boolean status = false;
	isBrowserOpen = false;
	isDhcpV4Configured = false;
	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	Map<String, String> webPaGetResponse = new HashMap<>();
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-WEBUI-706";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-7006");
	LOGGER.info(
		"TEST DESCRIPTION: Verify whether the CM can act as a DHCPv4 server for 2.4GHz Wi-Fi Client Scenario.");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1 : Obtain the Wi-Fi client 2.4 GHz associated with the gateway.");
	LOGGER.info("PRE-CONDITION 2 : Verify the correct IPV4 address for the Wi-Fi client.");
	LOGGER.info("PRE-CONDITION 3 : Verify the correct IPV6 address for the Wi-Fi client.");
	LOGGER.info(
		"PRE-CONDITION 4 : Verify the Internet Connectivity in the Connected Wi-Fi client using IPV4 interface.");
	LOGGER.info(
		"PRE-CONDITION 5 : Verify the Internet Connectivity in the Connected Wi-Fi client using IPV6 interface.");
	LOGGER.info(
		"PRE-CONDITION 6 : Get Default DHCP beginning address ,DHCP ending address ,DHCP lease time Using Webpa");
	LOGGER.info("1. Launch Broad band WebUI login page and verify login status");
	LOGGER.info("2. Launch the Local IP Network page from AdminUI page");
	LOGGER.info("3. Verify and Set the random DHCP IPV4 beginning address in Local IP Network Page.  ");
	LOGGER.info("4. Verify and Set the random DHCP IPV4 ending address in Local IP Network Page.  ");
	LOGGER.info("5. Verify the new DHCPv4 configuration setting are saved in the Local IP Network page.");
	LOGGER.info(
		"6. Change DHCP lease time to 2 minutes  of Connected client 2 using SNMP oid(1.3.6.1.4.1.1429.50.2.3.3.1.5.32)");
	LOGGER.info(
		"7. Verify and Retrive the new DHCPv4 configuration for beginning and ending address is set using WEBPA.");
	LOGGER.info("8. Verify IPv4 is assigned on the Wi-Fi client");
	LOGGER.info("9. Verify the internet connectivity in the Wi-Fi client connected using ipv4 interface.");
	LOGGER.info("POST-CONDITION : Close the open browser.");
	LOGGER.info("POST-CONDITION : Revert the DHCP configurations to default values.");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    deviceConnectedWith2_4GhzWifi = BroadBandPreConditionUtils
		    .executePreConditionToVerifyWiFiClientStatus(device, tapEnv, BroadBandTestConstants.BAND_2_4GHZ);
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 6: DESCRIPTION :Get DHCP beginning address ,DHCP ending  address ,DHCP lease time Using Webpa");
	    LOGGER.info(
		    "PRE-CONDITION 6: ACTION: Execute Webpa command with Parameter :Device.DHCPv4.Server.Pool.1.MinAddress,Device.DHCPv4.Server.Pool.1.MaxAddress,Device.DHCPv4.Server.Pool.1.LeaseTime ");
	    LOGGER.info("PRE-CONDITION 6: EXPECTED: Webpa get request should be successful");
	    String[] webPaParametersForDhcp = {
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME };
	    webPaGetResponse = tapEnv.executeMultipleWebPaGetCommands(device, webPaParametersForDhcp);
	    broadBandResultObject = BroadBandWebPaUtils.verifyDhcpv4OutputValues(webPaGetResponse);
	    status = broadBandResultObject.isStatus();
	    errorMessage = broadBandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info("PRE-CONDITION 6 : ACTUAL : Pre condition executed successfully");
	    } else {
		LOGGER.error("PRE-CONDITION 6 : ACTUAL : Pre condition failed");
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 6 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    LOGGER.info("**********************************************************************************");

	    verifyDhcpV4ConfigurationChangesThroughWebGui(device, testCaseId, deviceConnectedWith2_4GhzWifi);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    if (isBrowserOpen) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info("POST-CONDITION 1: DESCRIPTION : Close the browser.");
		LOGGER.info("POST-CONDITION 1: EXPECTED : Browser should be closed successfully.");
		try {
		    LanWebGuiLoginPage.closeBrowser();
		    LOGGER.info("POST-CONDITION 1: ACTUAL : Browser closed successfully");
		} catch (Exception exception) {
		    LOGGER.error(
			    "POST-CONDITION 1: ACTUAL : Exception occurred while closing the browser, unable to close browser.");
		}
	    }
	    if (isDhcpV4Configured) {
		BroadBandPostConditionUtils.executePostConditionToRevertDhcpv4ConfigurationToDefaultValues(device,
			tapEnv, webPaGetResponse, BroadBandTestConstants.STRING_CONSTANT_2);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-7006");
   }
   

   /**
    *
    * Test Case : Verify whether the CM can act as a DHCPv4 server for 5GHz Wi-Fi Client Scenario.
    *
    * <p>
    * STEPS:
    * </p>
    * <ol>
    * <li>PRE-CONDITION 1 : Obtain the Wi-Fi client 5GHz associated with the gateway.</li>
    * <li>PRE-CONDITION 2 : Verify the correct IPV4 address for the Wi-Fi client.</li>
    * <li>PRE-CONDITION 3 : Verify the correct IPV6 address for the Wi-Fi client.</li>
    * <li>PRE-CONDITION 4 : Verify the Internet Connectivity in the Connected Wi-Fi client using IPV4 interface.</li>
    * <li>PRE-CONDITION 5 : Verify the Internet Connectivity in the Connected Wi-Fi client using IPV6 interface.</li>
    * <li>PRE-CONDITION 6 : Get Default DHCP beginning address ,DHCP ending address ,DHCP lease time Using Webpa</li>
    * <li>Step 1: Launch Broad band WebUI login page and verify login status</li>
    * <li>Step 2: Launch the Local IP Network page from AdminUI page</li>
    * <li>Step 3: Verify and Set the random DHCP IPV4 beginning address in Local IP Network Page.</li>
    * <li>Step 4: Verify and Set the random DHCP IPV4 ending address in Local IP Network Page.</li>
    * <li>Step 5: Verify the new DHCPv4 configuration setting are saved in the Local IP Network page.</li>
    * <li>Step 6: Change DHCP lease time to 2 minutes of Connected client 2 using SNMP
    * oid(1.3.6.1.4.1.1429.50.2.3.3.1.5.32)</li>
    * <li>Step 7: Verify and Retrieve the new DHCPv4 configuration for beginning and ending address is set using
    * WEBPA.</li>
    * <li>Step 8: Verify IPv4 is assigned on the Wi-Fi client</li>
    * <li>Step 9: Verify the Internet Connectivity in the connected Wi-Fi client using ipv4 interface.</li>
    * <li>Post-Condition : Close the Browser.</li>
    * <li>Post-Condition : Revert the DHCP configurations to default values.</li>
    * </ol>
    * 
    * @param device
    *            {@link Dut}
    * @author Joseph M
    * @refactor Athira
    */
   @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
   @TestDetails(testUID = "TC-RDKB-WEBUI-7008")

   public void setDhcpMinMaxRangeUsing5GhzWifiClient(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	Dut deviceConnectedWith5GhzWifi = null; // connected device to be verified
	String errorMessage = "unable to obtain 5Ghz client or Client configuration is invalid";
	boolean status = false;
	isBrowserOpen = false;
	isDhcpV4Configured = false;
	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	Map<String, String> webPaGetResponse = new HashMap<>();
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-WEBUI-708";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-7008");
	LOGGER.info(
		"TEST DESCRIPTION: Verify whether the CM can act as a DHCPv4 server for 5GHz Wi-Fi Client Scenario.");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1 : Obtain the Wi-Fi client 5GHz associated with the gateway.");
	LOGGER.info("PRE-CONDITION 2 : Verify the correct IPV4 address for the Wi-Fi client.");
	LOGGER.info("PRE-CONDITION 3 : Verify the correct IPV6 address for the Wi-Fi client.");
	LOGGER.info(
		"PRE-CONDITION 4 : Verify the Internet Connectivity in the Connected Wi-Fi client using IPV4 interface.");
	LOGGER.info(
		"PRE-CONDITION 5 : Verify the Internet Connectivity in the Connected Wi-Fi client using IPV6 interface.");
	LOGGER.info(
		"PRE-CONDITION 6 : Get Default DHCP beginning address ,DHCP ending address ,DHCP lease time Using Webpa");
	LOGGER.info("1. Launch Broad band WebUI login page and verify login status");
	LOGGER.info("2. Launch the Local IP Network page from AdminUI page");
	LOGGER.info("3. Verify and Set the random DHCP IPV4 beginning address in Local IP Network Page.  ");
	LOGGER.info("4. Verify and Set the random DHCP IPV4 ending address in Local IP Network Page.  ");
	LOGGER.info("5. Verify the new DHCPv4 configuration setting are saved in the Local IP Network page.");
	LOGGER.info(
		"6. Change DHCP lease time to 2 minutes  of Connected client 2 using SNMP oid(1.3.6.1.4.1.1429.50.2.3.3.1.5.32)");
	LOGGER.info(
		"7. Verify and Retrive the new DHCPv4 configuration for beginning and ending address is set using WEBPA.");
	LOGGER.info("8. Verify IPv4 is assigned on the Wi-Fi client");
	LOGGER.info("9. Verify the internet connectivity in the Wi-Fi client connected using ipv4 interface.");
	LOGGER.info("POST-CONDITION : Close the open browser.");
	LOGGER.info("POST-CONDITION : Revert the DHCP configurations to default values.");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    deviceConnectedWith5GhzWifi = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
		    tapEnv, BroadBandTestConstants.BAND_5GHZ);
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 6: DESCRIPTION :Get DHCP beginning address ,DHCP ending  address ,DHCP lease time Using Webpa");
	    LOGGER.info(
		    "PRE-CONDITION 6: ACTION: Execute Webpa command with Parameter :Device.DHCPv4.Server.Pool.1.MinAddress,Device.DHCPv4.Server.Pool.1.MaxAddress,Device.DHCPv4.Server.Pool.1.LeaseTime ");
	    LOGGER.info("PRE-CONDITION 6: EXPECTED: Webpa get request should be successful");
	    String[] webPaParametersForDhcp = {
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME };
	    webPaGetResponse = tapEnv.executeMultipleWebPaGetCommands(device, webPaParametersForDhcp);
	    broadBandResultObject = BroadBandWebPaUtils.verifyDhcpv4OutputValues(webPaGetResponse);
	    status = broadBandResultObject.isStatus();
	    errorMessage = broadBandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info("PRE-CONDITION 6 : ACTUAL : Pre condition executed successfully");
	    } else {
		LOGGER.error("PRE-CONDITION 6 : ACTUAL : Pre condition failed");
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    LOGGER.info("**********************************************************************************");

	    verifyDhcpV4ConfigurationChangesThroughWebGui(device, testCaseId, deviceConnectedWith5GhzWifi);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    if (isBrowserOpen) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info("POST-CONDITION 1: DESCRIPTION : Close the browser.");
		LOGGER.info("POST-CONDITION 1: EXPECTED : Browser should be closed successfully.");
		try {
		    LanWebGuiLoginPage.closeBrowser();
		    LOGGER.info("POST-CONDITION 1: ACTUAL : Browser closed successfully");
		} catch (Exception exception) {
		    LOGGER.error(
			    "POST-CONDITION 1: ACTUAL : Exception occurred while closing the browser, unable to close browser.");
		}
	    }
	    if (isDhcpV4Configured) {
		BroadBandPostConditionUtils.executePostConditionToRevertDhcpv4ConfigurationToDefaultValues(device,
			tapEnv, webPaGetResponse, BroadBandTestConstants.STRING_CONSTANT_2);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-7008");
   }
   
   /**
    * Method to Set DHCPV4 Configuration to Random Values Via LanUI
    * 
    * @param device
    *            {@link Dut}
    * @param testCaseId
    *            Instance for Test Case ID.
    * @param deviceConnected
    *            Instance for Device Connected.
    *
    * @refactor Athira
    * 
    */
   public void verifyDhcpV4ConfigurationChangesThroughWebGui(Dut device, String testCaseId, Dut deviceConnected) {
	// Variable Declaration begins
	boolean status = false;
	WebDriver webDriver = null;
	BroadbandLocalIpConfigurationPage localIpPage = null; // stores the common page object
	int dhcpIpv4BeginningValue = 0;
	int dhcpIpv4EndingValue = 0;
	WebElement element = null;
	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	List<Integer> dhcpBeginningAndEndingValue = new ArrayList<Integer>();
	BroadBandResultObject result = null;
	// Variable Declaration Ends

	stepNum = "S1";
	errorMessage = "Unable to Login to LanGUI page using Admin credential";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP 1: DESCRIPTION : Launch Broad band WebUI login page and verify login status");
	LOGGER.info(
		"STEP 1: ACTION : Launch the below URL format in browserhttps://10.0.0.1/LOGIN CREDENTIALS :  username: adminPassword: password");
	LOGGER.info("STEP 1: EXPECTED : Public Wifi page should be launched and login should be successful");
	LOGGER.info("**********************************************************************************");
	status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnected);
	isBrowserOpen = status;
	webDriver = LanWebGuiLoginPage.getDriver();
	if (status) {
	    LOGGER.info("STEP 1: ACTUAL : Launch Broad band LanUI login page and verify login status is successful");
	} else {
	    LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNum = "S2";
	errorMessage = "UNABLE TO LAUNCH THE 'GATEWAY' > 'CONNECTIONS' > 'Local IP Configuration' Page from Admin UI Page.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP 2: DESCRIPTION : Launch the Local IP Network page from AdminUI page");
	LOGGER.info(
		"STEP 2: ACTION : Navigate to  Local IP Network page by clicking on \"Connection\" Menu and then select the submenu \"Local Ip Network\".");
	LOGGER.info(
		"STEP 2: EXPECTED : Local IP Network page should get displayed having page title as \"Gateway > Connection > Local IP Configuration\"");
	LOGGER.info("**********************************************************************************");
	localIpPage = new BroadbandLocalIpConfigurationPage(webDriver);
	status = localIpPage.navigateToLocalIpPage(tapEnv, device);
	if (status) {
	    LOGGER.info(
		    "STEP 2: ACTUAL : Navigating to  Local IP Network page by clicking on \"Connection\" Menu and then select the submenu \"Local Ip Network\" is successful.");
	} else {
	    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNum = "S3";
	errorMessage = "Unable to Set the DHCP IPV4 beginning address in the 'Local IP Network' page in LanGUI Page.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 3: DESCRIPTION : Verify and Set the random DHCP IPV4 beginning address in Local IP Network Page.  ");
	LOGGER.info(
		"STEP 3: ACTION :  The DHCPv4 Beginning Address Text box should be enabled and should be set with the new Beginning Address values.");
	LOGGER.info(
		"STEP 3: EXPECTED : DHCPv4 Beginning Address should be set with the new beginning Address value (Eg.10.0.0.20 )");
	LOGGER.info("**********************************************************************************");
	try {
	    dhcpBeginningAndEndingValue = BroadBandCommonUtils.getRandomDhcpV4BeginningAndEndingAddress();
	    dhcpIpv4BeginningValue = dhcpBeginningAndEndingValue.get(BroadBandTestConstants.CONSTANT_0);
	    element = webDriver
		    .findElement(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_BEGINNING_ADDRESS));
	    element.clear();
	    if (dhcpIpv4BeginningValue > BroadBandTestConstants.CONSTANT_1
		    && dhcpIpv4BeginningValue < BroadBandTestConstants.CONSTANT_253) {
		LOGGER.info("DHCP IPV4 Beginning Value set in UI is: " + dhcpIpv4BeginningValue);
		element.sendKeys(String.valueOf(dhcpIpv4BeginningValue));
		status = true;
	    } else {
		errorMessage = "DHCP IPV4 Beginning and Ending Value are not in valid range.";
	    }
	} catch (NoSuchElementException noSuchElementException) {
	    LOGGER.info(noSuchElementException.getMessage());
	    LOGGER.info("Element is not present in UI");
	}
	if (status) {
	    LOGGER.info(
		    "STEP 3: ACTUAL : DHCPv4 Beginning Address is set with the new beginning Address value successfully.");
	} else {
	    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNum = "S4";
	errorMessage = "Unable to Set the DHCP IPV4 Ending address in the 'Local IP Network' page in LanGUI Page.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 4: DESCRIPTION : Verify and Set the random DHCP IPV4 ending address in Local IP Network Page.  ");
	LOGGER.info(
		"STEP 4: ACTION :  The DHCPv4 Ending Address Text box should be enabled and should be set with the new Ending Address values.");
	LOGGER.info(
		"STEP 4: EXPECTED : DHCPv4 Ending Address should be set with the new Ending Address value (Eg.10.0.0.25 )");
	LOGGER.info("**********************************************************************************");
	try {
	    dhcpIpv4EndingValue = dhcpBeginningAndEndingValue.get(BroadBandTestConstants.CONSTANT_1);
	    element = webDriver.findElement(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_ENDING_ADDRESS));
	    element.clear();
	    if (dhcpIpv4EndingValue > dhcpIpv4BeginningValue
		    && dhcpIpv4EndingValue < BroadBandTestConstants.CONSTANT_253) {
		LOGGER.info("DHCP IPV4 Ending Value set in UI is: " + dhcpIpv4EndingValue);
		element.sendKeys(String.valueOf(dhcpIpv4EndingValue));
		status = true;
	    }
	} catch (NoSuchElementException noSuchElementException) {
	    LOGGER.info(noSuchElementException.getMessage());
	    LOGGER.info("Element is not present in UI");
	}
	if (status) {
	    LOGGER.info(
		    "STEP 4: ACTUAL : DHCPv4 Ending Address is set with the new beginning Address value successfully.");
	} else {
	    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNum = "S5";
	errorMessage = "Unable to save the DHCP IPV4 server setting from Local IP Network Page.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 5: DESCRIPTION : Verify the new DHCPv4 configuration setting are saved in the Local IP Network page.");
	LOGGER.info(
		"STEP 5: ACTION :  The SAVE SETTINGS button under IPv4 from the Local IP Network page should be enabled and clicked .");
	LOGGER.info(
		"STEP 5: EXPECTED : The DHCPv4 server settings should be saved by clicking the save button in Local IP Network page.");
	LOGGER.info("**********************************************************************************");
	try {
	    element = webDriver.findElement(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_SAVE_BUTTON));
	    element.click();
	    LOGGER.info("Clicking on Ok button to accept Pop Up.");
	    status = true;
	} catch (NoSuchElementException noSuchElementException) {
	    LOGGER.info(noSuchElementException.getMessage());
	    LOGGER.info("Element is not present in UI");
	}
	if (status) {
	    LOGGER.info("STEP 5: ACTUAL : The DHCPv4 server settings is successfully saved in Local IP Network page.");
	    isDhcpV4Configured = true;
	    LOGGER.info("Waiting for 2 minutes for the changes to apply.");
	    tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
	} else {
	    LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNum = "s6";
	errorMessage = "Failed to set lease time to 120 seconds";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP 6 : DESCRIPTION :Change DHCP lease time to 2 minutes");
	LOGGER.info(
		"STEP 6 : ACTION: Execute Webpa command with Parameter :Device.DHCPv4.Server.Pool.1.LeaseTime Value:120 ");
	LOGGER.info("STEP 6 : EXPECTED: DHCP lease time values should be changes succssfully to 2 minutes");
	LOGGER.info("**********************************************************************************");
	status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
		BroadBandTestConstants.STRING_LEASE_TIME_VALUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	if (status) {
	    LOGGER.info("Waiting for 2 minutes to reflect");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    LOGGER.info("STEP 6:ACTUAL :DHCP Lease time changes to 2 minutes successfully ");
	} else {
	    LOGGER.error("STEP 6:ACTUAL :" + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNum = "S7";
	errorMessage = "Unable to retrive the DHCPv4 beginning and Ending address assigned to the client Connected via WEBPA.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 7: DESCRIPTION : Verify and Retrive the new DHCPv4 configuration for beginning and ending address is set using WEBPA.");
	LOGGER.info("STEP 7: ACTION : Retrive the min and max Range of DHCP in the connected client.");
	LOGGER.info(
		"STEP 7: EXPECTED : Retriving and verifying the new DHCPv4 configuration for beginning and ending address should be successful using WEBPA.");
	LOGGER.info("**********************************************************************************");
	status = BroadBandCommonUtils.verifyMinMaxRangeSetUsingWebpa(tapEnv, device, dhcpIpv4BeginningValue,
		dhcpIpv4EndingValue);
	if (status) {
	    LOGGER.info(
		    "STEP 7: ACTUAL : Retrieving and verifying the beginning and ending values of DHCPv4 was successful using WEBPA.");
	} else {
	    LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
		errorMessage, true);

	stepNum = "S8";
	errorMessage = "Unable to retrive the IPV4 assigned to the client Connected.";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP 8: DESCRIPTION : Verify IPv4 is assigned on the Connected client");
	LOGGER.info(
		"STEP 8: ACTION : Get the device IPv4 address using below commandLinux : ifconfig wlan0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"Pv4 Address\" and check whether the ipv4 address obtained is between dhcp min range and Dhcp Max range");
	LOGGER.info(
		"STEP 8: EXPECTED : Connected Client should return the IPv4 Address between the configured Minimum and Maximum Address range(Ex.10.0.0.20 and 10.0.0.25) ");
	LOGGER.info("**********************************************************************************");
	broadBandResultObject = BroadBandConnectedClientUtils.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device,
		tapEnv, deviceConnected);

	/*
	 * status = BroadBandConnectedClientUtils. verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
	 * deviceConnected);
	 */

	status = broadBandResultObject.isStatus();
	errorMessage = broadBandResultObject.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP 8: ACTUAL : IPV4 is assigned between the new minimum and maximum DHCP IPV4 range set.");
	} else {
	    LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNum = "S9";
	errorMessage = "NOT ABLE TO ACCESS THE SITE 'www.google.com' FROM LAN CLIENT WITH USING IPV4";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 9 : DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED CLIENT USING IPV4 INTERFACE");
	LOGGER.info(
		"STEP 9 : ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -4 -n 5 google.com ON THE CONNECTED LAN CLIENT");
	LOGGER.info("STEP 9 : EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4 ");
	LOGGER.info("**********************************************************************************");
	try {
	    result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		    deviceConnected,
		    BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
		    BroadBandTestConstants.IP_VERSION4);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (!status) {
		errorMessage = "PIGN OPERATION FAILED TO ACCESS THE SITE 'www.google.com' USING IPV4 ";
		status = ConnectedNattedClientsUtils.verifyPingConnectionForIpv4AndIpv6(deviceConnected, tapEnv,
			BroadBandTestConstants.PING_TO_GOOGLE, BroadBandTestConstants.IP_VERSION4);
	    }
	} catch (Exception e) {
	    LOGGER.info(e.getMessage());
	}
	if (status) {
	    LOGGER.info("STEP 9 : ACTUAL : CONNECTED CLIENT HAS INTERNET CONNECTIVITY USING IPV4 INTERFACE");
	} else {
	    LOGGER.error("STEP 9 : ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
   }

   /**
    * Verify Internet access for clients after setting its assigned DHCP IP as reserved IP
    * <ol>
    * <li>Connect the client to Private Wi-Fi Network and verify connection status</li>
    * <li>Verify the client connected to Private Wi-Fi Network has got the IPv4 Address between DHCP Range</li>
    * <li>Verify the client connected to Private Wi-Fi Network has got the IPv6 Address</li>
    * <li>Verify the internet is accessible in the client connected to the Private Wi-Fi Network</li>
    * <li>Verify the Wi-Fi MAC address of connected client is retrieved successfully</li>
    * <li>Connect the Ethernet client associated with the gateway and verify connection status</li>
    * <li>Verify the Ethernet client connected has got the IPv4 Address between DHCP Range</li>
    * <li>Verify the Ethernet client connected has got the IPv6 Address</li>
    * <li>Verify the internet is accessible in the client connected to the Ethernet Client</li>
    * <li>Verify the Ethernet MAC address of connected client is retrieved successfully</li>
    * <li>Launch Broad band User Admin page login page and verify login status</li>
    * <li>Launch the Connected Devices page from public wifi page</li>
    * <li>Verify the Connected Client with Wifi Mac Address is Listed which is obtained in step5 and Click on Edit and
    * Verify the Page Title as "Connected Devices >Devices>Edit Devices"</li>
    * <li>Verify Selecting the Reserved IP Radio Button and verify client IPV4 Obtained in step 2 should be populated
    * in the Text Box and Click on Save Button.</li>
    * <li>Verify the Connected Client with Ethernet Mac Address is Listed which is obtained in step10 and Click on Edit
    * and Verify the Page Title as "Connected Devices >Devices>Edit Devices"</li>
    * <li>Verify Selecting the Reserved IP Radio Button and verify client IPV4 Obtained in step 7 should be populated
    * in the Text Box and Click on Save Button.</li>
    * <li>Verify the internet is accessible in the client connected to the Private Wi-Fi Network</li>
    * <li>Verify the internet is accessible in the client connected to Ethernet</li>
    * </ol>
    * 
    * @author Gnanaprakasham S
    * @refactor Rakesh C N
    */
   @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WEBGUI)
   @TestDetails(testUID = "TC-RDKB-RESERVEDIP-5011")
   public void testToSetReservedIpAndVerifyInternetAccess(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-RESERVEDIP-511";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	Dut wirelessConnectedClientSettop = null;
	Dut wiredConnectedClientSettop = null;
	String ipv4AddressretrievedFromWirelessClient = "";
	String ipv4AddressretrievedFromWiredClient = "";
	String macAddressOfWiredConnClient = "";
	boolean isBrowserOpen = false;
	boolean isEthernetClientAvailable = false;
	WebDriver webDriver = null;

	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RESERVEDIP-5011");
	LOGGER.info(
		"TEST DESCRIPTION: Verify Internet access for clients after setting its assigned DHCP IP as reserved IP");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Connect the  client to Private Wi-Fi Network and verify connection status ");
	LOGGER.info(
		"2. Verify the client connected to Private Wi-Fi Network has got the IPv4 Address between DHCP Range");
	LOGGER.info("3. Verify the client connected to Private Wi-Fi Network has got the IPv6 Address");
	LOGGER.info("4. Verify the internet is accessible in the client connected to the Private Wi-Fi Network");
	LOGGER.info("5. Verify the Wi-Fi MAC address of connected client is retrieved successfully");
	LOGGER.info("6. Connect the  Ethernet client associated with the gateway and verify connection status ");
	LOGGER.info("7. Verify the  Ethernet client connected  has got the IPv4 Address between DHCP Range");
	LOGGER.info("8. Verify the Ethernet client connected has got the IPv6 Address");
	LOGGER.info("9. Verify the internet is accessible in the client connected to the Ethernet Client");
	LOGGER.info("10. Verify the Ethernet MAC address of connected client is retrieved successfully");
	LOGGER.info("11. Launch Broad band User Admin page login page and verify login status");
	LOGGER.info("12. Launch the Connected Devices page from public wifi page");
	LOGGER.info(
		"13. Verify the Connected Client with Wifi Mac Address is Listed which is obtained in step5 and Click on Edit and Verify the Page Title as \"Connected Devices >Devices>Edit Devices\"");
	LOGGER.info(
		"14. Verify Selecting the Reserved IP Radio Button and verify client IPV4 Obtained in step 2  should be populated in the Text Box and Click on Save Button.");
	LOGGER.info(
		"15. Verify the Connected Client with  Ethernet Mac Address is Listed which is obtained in step10 and Click on Edit and Verify the Page Title as \"Connected Devices >Devices>Edit Devices\"");
	LOGGER.info(
		"16. Verify Selecting the Reserved IP Radio Button and verify client IPV4 Obtained in step 7  should be populated in the Text Box and Click on Save Button.");
	LOGGER.info("17. Verify the internet is accessible in the client connected to the Private Wi-Fi Network");
	LOGGER.info("18. Verify the internet is accessible in the client connected to Ethernet");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Verify the DHCPv4 lease time can be set to 2 minutes");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION : Execute Webpa command with Parameter :Device.DHCPv4.Server.Pool.1.LeaseTime Value:120");
	    LOGGER.info("PRE-CONDITION : EXPECTED : DHCPv4 lease time should be changed successfully to 2 minutes");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
		    BroadBandTestConstants.STRING_LEASE_TIME_VALUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("PRE-CONDITION : ACTUAL : DHCPV4 LEASE TIME IS SUCCESSFULLY SET TO 2 MINUTES");
	    } else {
		LOGGER.error("PRE-CONDITION : ACTUAL : FAILED TO SET LEASE TIME TO 120 SECONDS");
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ " FAILED TO SET LEASE TIME TO 120 SECONDS - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

	    stepNum = "S1";
	    status = false;
	    errorMessage = "Unable to connect to the private Wi-Fi Network";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Connect a client to Private Wi-Fi Network and verify connection status ");
	    LOGGER.info(
		    "STEP 1: ACTION : Connect to Private Wi-Fi using below commandsLinux :nmcli dev wifi connect <ssid> password <passwd>Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP 1: EXPECTED : Device should be connected with  Private Wi-Fi Network");
	    LOGGER.info("**********************************************************************************");
	    try {
		wirelessConnectedClientSettop = BroadBandConnectedClientUtils
			.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = null != wirelessConnectedClientSettop;
	    if (status) {
		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info("STEP 1: ACTUAL : Device has been connected with the Private Wi-Fi Network");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    // step-2 to step-4
	    List<String> ipAddresses = BroadBandConnectedClientUtils
		    .validateIpAddressesAndInternetConnectivityOfConnectedClient(device, wirelessConnectedClientSettop,
			    tapEnv, BroadBandTestConstants.CONSTANT_2, testCaseId);

	    for (String ipAddress : ipAddresses) {
		if (CommonMethods.isIpv4Address(ipAddress)) {
		    ipv4AddressretrievedFromWirelessClient = ipAddress;
		}
	    }

	    stepNum = "S5";
	    errorMessage = "Unable to retrieve the Wi-Fi MAC address of the connected client OR Wi-Fi MAC Address is not configured properly in MDS/Inventory";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify the Wi-Fi MAC address of connected client is retrieved successfully");
	    LOGGER.info("STEP 5: ACTION : Get the WiFi MAC Address of the connected client from CATS Inventory");
	    LOGGER.info(
		    "STEP 5: EXPECTED : WiFi MAC address of the connected client should be retrieved successfully from MDS");
	    LOGGER.info("**********************************************************************************");
	    String macAddressOfWirelessConnClient = ((Device) wirelessConnectedClientSettop).getConnectedDeviceInfo()
		    .getWifiMacAddress();
	    LOGGER.info("WI-FI MAC ADDRESS OF THE CONNECTED CLIENT OBTAINED IS : " + macAddressOfWirelessConnClient);
	    status = CommonMethods.isNotNull(macAddressOfWirelessConnClient)
		    && CommonMethods.isMacValid(macAddressOfWirelessConnClient);
	    if (status) {
		macAddressOfWirelessConnClient = macAddressOfWirelessConnClient.toUpperCase()
			.replace(AutomaticsConstants.COLON, BroadBandTestConstants.EMPTY_STRING);
		LOGGER.info(
			"S5 ACTUAL : Successfully retrieved the Wifi Mac address of the connected client having 2.4GHZ wifi Capability");
	    } else {
		LOGGER.error("S5 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S6";
	    status = false;
	    errorMessage = "Unable to retrieve the client connected to Ethernet OR No ethernet available with this setup";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Connect the Ethernet client associated with the gateway and verify connection status ");
	    LOGGER.info("STEP 6: ACTION : Obtain a Ethernet Client assocaited with the gateway");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Connecting the  Ethernet client associated with the gateway and verify connection status should be successful.");
	    LOGGER.info("**********************************************************************************");
	    wiredConnectedClientSettop = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
	    status = null != wiredConnectedClientSettop;
	    isEthernetClientAvailable = status;
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : #successMessage#");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    if (isEthernetClientAvailable) {

		// step-7 to step-9
		ipAddresses = BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(
			device, wiredConnectedClientSettop, tapEnv, BroadBandTestConstants.CONSTANT_7, testCaseId);

		for (String ipAddress : ipAddresses) {
		    if (CommonMethods.isIpv4Address(ipAddress)) {
			ipv4AddressretrievedFromWiredClient = ipAddress;
		    }
		}

		stepNum = "S10";
		errorMessage = "Unable to retrieve the Ethernet Mac address of the connected client OR Ethernet Mac Address is not configured properly in MDS/Inventory";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 10: DESCRIPTION : Verify the Ethernet MAC address of connected client is retrieved successfully");
		LOGGER.info(
			"STEP 10: ACTION : Get theEthernet MAC Address of the connected client from Inventory");
		LOGGER.info(
			"STEP 10: EXPECTED : Ethernet MAC address of the connected client should be retrieved successfully from MDS");
		LOGGER.info("**********************************************************************************");
		macAddressOfWiredConnClient = ((Device) wiredConnectedClientSettop).getConnectedDeviceInfo()
			.getEthernetMacAddress();
		LOGGER.info(
			"ETHERNET MAC ADDRESS OF THE CONNECTED CLIENT OBTAINED IS : " + macAddressOfWiredConnClient);
		status = CommonMethods.isNotNull(macAddressOfWiredConnClient)
			&& CommonMethods.isMacValid(macAddressOfWiredConnClient);
		if (status) {
		    macAddressOfWiredConnClient = macAddressOfWiredConnClient.toUpperCase()
			    .replace(AutomaticsConstants.COLON, BroadBandTestConstants.EMPTY_STRING);
		    LOGGER.info("S10 ACTUAL : Successfully retrieved the Ethernet MAC address of the connected client");
		} else {
		    LOGGER.error("S10 ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    } else {
		LOGGER.error("SINCE ETHERNET CLIENT IS NOT AVAILABLE, SKIPPING STEP #7 to STEP #10");
	    }

	    // step-11
	    status = LanSidePageNavigation.executeTestStepToLoginLanAdminWebGui(device, tapEnv,
		    wirelessConnectedClientSettop, testCaseId, 11);
	    if (status) {
		isBrowserOpen = status;
		webDriver = LanWebGuiLoginPage.getDriver();
	    }

	    stepNum = "S12";
	    errorMessage = "Unable to navigate Connected Devices page";
	    status = false;
	    LanSidePageNavigation lanSidePageNavigation = new LanSidePageNavigation(webDriver);
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Launch the Connected Devices page from public wifi page");
	    LOGGER.info("STEP 12: ACTION : Navigate to Devices  page by clicking on \"Connected Devices\" Menu ");
	    LOGGER.info(
		    "STEP 12: EXPECTED : Devices details page should get displayed having page title as \"Connected Devices >Devices\"");
	    LOGGER.info("**********************************************************************************");
	    status = lanSidePageNavigation.navigateToConnectedDevicesPage(device, tapEnv, webDriver);
	    if (status) {
		LOGGER.info(
			"STEP 12: ACTUAL : Connected Devices page has been launched successfully having Page Title as Gateway > Connected Devices");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S13";
	    status = false;
	    errorMessage = "Unable to configure assigned DHCP IP as reserved IP for Wireless Connected Client";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : Verify the Wireless Connected Client can be configured with its assigned DHCP IP as reserved IP");
	    LOGGER.info(
		    "STEP 13: ACTION : Click the Edit Button from the connected Devices Page for the Wifi Mac of the Connected Client Obtained in Step 5 and configure reserved IP and click on 'Save'");
	    LOGGER.info(
		    "STEP 13: EXPECTED : Wireless Connected Client should be successfully configured with its assigned DHCP IP as reserved IP");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonPage.modifyClientDhcpConfiguration(tapEnv, webDriver, true,
		    macAddressOfWirelessConnClient, ipv4AddressretrievedFromWirelessClient);
	    if (status) {
		LOGGER.info(
			"STEP 13: ACTUAL : Wireless Connected Client is successfully configured with its assigned DHCP IP as reserved IP");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    if (isEthernetClientAvailable) {

		stepNum = "S14";
		status = false;
		errorMessage = "Unable to configure assigned DHCP IP as reserved IP for Wired Connected Client";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 14: DESCRIPTION : Verify the Wired Connected Client can be configured with its assigned DHCP IP as reserved IP");
		LOGGER.info(
			"STEP 14: ACTION : Click the Edit Button from the connected Devices Page for the Ethernet Mac of the Connected Client Obtained in Step 10 and configure reserved IP and click on 'Save'");
		LOGGER.info(
			"STEP 14: EXPECTED : Wired Connected Client should be successfully configured with its assigned DHCP IP as reserved IP");
		LOGGER.info("**********************************************************************************");
		status = BroadBandCommonPage.modifyClientDhcpConfiguration(tapEnv, webDriver, true,
			macAddressOfWiredConnClient, ipv4AddressretrievedFromWiredClient);
		if (status) {
		    LOGGER.info(
			    "STEP 14: ACTUAL : Wired Connected Client is successfully configured with its assigned DHCP IP as reserved IP");
		} else {
		    LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
			status, errorMessage, false);

	    } else {
		errorMessage = "SINCE ETHERNET CLIENT IS NOT AVAILABLE, SKIPPING STEP #14";
		LOGGER.error(errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
			errorMessage, false);
	    }

	    stepNum = "S15";
	    status = false;
	    errorMessage = "Unable to check Internet access in the client connected to the Private Wi-Fi Network";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION : Verify the internet is accessible in the client connected to the Private Wi-Fi Network");
	    LOGGER.info(
		    "STEP 15: ACTION : Execute the command in connected client:curl --connect-timeout 20 --head https://www.wikipedia.org");
	    LOGGER.info("STEP 15: EXPECTED : Internet should be accessible in the connected client.");
	    LOGGER.info("**********************************************************************************");
	    BroadBandResultObject result = BroadBandConnectedClientUtils
		    .verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv, wirelessConnectedClientSettop,
			    BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.EMPTY_STRING);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 15: ACTUAL : Internet is accessible in the client connected to the Private Wi-Fi Network");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    if (isEthernetClientAvailable) {

		stepNum = "S16";
		status = false;
		result = null;
		errorMessage = "Unable to check Internet access in the client connected to Ethernet";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 16: DESCRIPTION : Verify the internet is accessible in the client connected to Ethernet");
		LOGGER.info(
			"STEP 16: ACTION : Execute the command in connected client:curl --connect-timeout 20 --head https://www.wikipedia.org");
		LOGGER.info("STEP 16: EXPECTED : Internet should be accessible in the connected client.");
		LOGGER.info("**********************************************************************************");
		result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
			wiredConnectedClientSettop, BroadBandTestConstants.URL_WIKIPEDIA,
			BroadBandTestConstants.EMPTY_STRING);
		status = result.isStatus();
		errorMessage = result.getErrorMessage();
		if (status) {
		    LOGGER.info("STEP 16: ACTUAL : Internet is accessible in the client connected to Ethernet");
		} else {
		    LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    } else {
		errorMessage = "SINCE ETHERNET CLIENT IS NOT AVAILABLE, SKIPPING STEP #16";
		LOGGER.error(errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
			errorMessage, false);
	    }

	    stepNum = "S17";
	    status = false;
	    errorMessage = "Unable to change configuration as DHCP from Reserved IP for Wireless Connected Client";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 17: DESCRIPTION : Verify the Wireless Connected Client can be set to DHCP configuration from Reserved IP");
	    LOGGER.info(
		    "STEP 17: ACTION : Click the Edit Button from the connected Devices Page for the Wifi Mac of the Connected Client Obtained in Step 5 and change the configuration to DHCP and click on 'Save'");
	    LOGGER.info(
		    "STEP 17: EXPECTED : Wireless Connected Client should be successfully set with DHCP configuration from Reserved IP");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonPage.modifyClientDhcpConfiguration(tapEnv, webDriver, false,
		    macAddressOfWirelessConnClient, ipv4AddressretrievedFromWirelessClient);
	    if (status) {
		LOGGER.info(
			"STEP 17: ACTUAL : Wireless Connected Client is successfully set with DHCP configuration from Reserved IP");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    if (isEthernetClientAvailable) {

		stepNum = "S18";
		status = false;
		errorMessage = "Unable to change configuration as DHCP from Reserved IP for Wired Connected Client";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 18: DESCRIPTION : Verify the Wired Connected Client can be set to DHCP configuration from Reserved IP");
		LOGGER.info(
			"STEP 18: ACTION : Click the Edit Button from the connected Devices Page for the Ethernet Mac of the Connected Client Obtained in Step 10 and change the configuration to DHCP and click on 'Save'");
		LOGGER.info(
			"STEP 18: EXPECTED : Wired Connected Client should be successfully set with DHCP configuration from Reserved IP");
		LOGGER.info("**********************************************************************************");
		status = BroadBandCommonPage.modifyClientDhcpConfiguration(tapEnv, webDriver, false,
			macAddressOfWiredConnClient, ipv4AddressretrievedFromWiredClient);
		if (status) {
		    LOGGER.info(
			    "STEP 18: ACTUAL : Wired Connected Client is successfully set with DHCP configuration from Reserved IP");
		} else {
		    LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
			status, errorMessage, true);

	    } else {
		errorMessage = "SINCE ETHERNET CLIENT IS NOT AVAILABLE, SKIPPING STEP #18";
		LOGGER.error(errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
			errorMessage, false);
	    }

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : VERIFY THE DHCPV4 LEASE TIME IS SUCCESSFULLY SET TO ITS DEFAULT VALUE");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : EXECUTE WEBPA COMMAND WITH PARAMETER :DEVICE.DHCPV4.SERVER.POOL.1.LEASETIME VALUE: 604800");
	    LOGGER.info(
		    "POST-CONDITION : EXPECTED : DHCPv4 LEASE TIME SHOULD BE SUCCESSFULLY SET TO ITS DEFAULT VALUE");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
		    BroadBandTestConstants.STRING_LEASE_TIME_DEFAULT_VALUE,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : DHCPV4 LEASE TIME IS SUCCESSFULLY SET TO ITS DEFAULT VALUE");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : UNABLE TO SET THE DHCPv4 LEASE TIME TO ITS DEFAULT VALUE");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);

	    if (isBrowserOpen) {
		BroadBandPostConditionUtils.postConditionCloseBrowser(isBrowserOpen, BroadBandTestConstants.CONSTANT_2);
	    }

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
   }

	/**
	 * Validate the error message when DHCP Starting and Ending address is invalid
	 * <ol>
	 * <li>Verify whether Private Wifi SSIDs\" are enabled using WebPA.</li>
	 * <li>Connect the client to Private Wi-Fi Network and verify connection
	 * status</li>
	 * <li>Verify the client connected to Private Wi-Fi Network has got the IPv4
	 * Address between DHCP Range</li>
	 * <li>Verify the client connected to Private Wi-Fi Network has got the IPv6
	 * Address</li>
	 * <li>Verify the internet is accessible in the client connected to the Private
	 * Wi-Fi Network</li>
	 * <li>Verify the gateway admin page on Wi-Fi client is launched and logged in
	 * successfully</li>
	 * <li>Launch the \"Connection\" page from AdminUI page</li>
	 * <li>Launch the \"Local IP Network\" page from \"Connection\" page</li>
	 * <li>Validate the error message when DHCP Starting address is larger than
	 * ending address</li>
	 * <li>Validate the error message when DHCP Ending address is invalid</li>
	 * </ol>
	 * 
	 * @refactor Athira
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-DHCP-NEG-5001")
	public void testToValidateLocalIpConfiguration(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-DHCP-NEG-501";
		String stepNum = "S1";
		String errorMessage = "";
		boolean status = false;
		LanSidePageNavigation lanSidePageNavigation = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-DHCP-NEG-5001");
		LOGGER.info("TEST DESCRIPTION: Validate the error message when DHCP Starting and Ending address is invalid");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify whether Private Wifi SSIDs\" are enabled using WebPA.");
		LOGGER.info("2. Connect the client to Private Wi-Fi Network and verify connection status ");
		LOGGER.info(
				"3. Verify the client connected to Private Wi-Fi Network has got the IPv4 Address between DHCP Range");
		LOGGER.info("4. Verify the client connected to Private Wi-Fi Network has got the IPv6 Address");
		LOGGER.info("5. Verify the internet is accessible in the client connected to the Private Wi-Fi Network");
		LOGGER.info("6. Verify the gateway admin page on Wi-Fi  client is launched and logged in successfully");
		LOGGER.info("7. Launch the \"Connection\" page from AdminUI page");
		LOGGER.info("8. Launch the \"Local IP Network\" page from \"Connection\" page");
		LOGGER.info("9. Validate the error message when DHCP Starting address is larger than ending address");
		LOGGER.info("10. Validate the error message when DHCP Ending address is invalid");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "Enabling Private Wi-Fi SSIDs' via WebPA failed";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify whether Private Wifi SSIDs\" are enabled using WebPA.");
			LOGGER.info(
					"STEP 1: ACTION : Execute the command:FOR 2.4 GHZ: curl -X PATCH <WEBPA URL>/api/v2/device/mac:<ECM MAC>/config -d \"{\"parameters\":[{\"name\":\"Device.WiFi.SSID.10001.Enable\",\"value\":\"True\",\"dataType\":3}]}\"  FOR 5 GHZ: curl -X PATCH <WEBPA URL>/api/v2/device/mac:<ECM MAC>/config -d \"{\"parameters\":[{\"name\":\"Device.WiFi.SSID.10101.Enable\",\"value\":\"True\",\"dataType\":3}]}\"      ");
			LOGGER.info("STEP 1: EXPECTED : Both 2.4 GHz and 5 GHz radio should be enabled");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Enabling 2.4 GHz private Wi-Fi radio via WebPA failed";
			if (BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
					WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, true)) {
				status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
						WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
				errorMessage = "Enabling 5 GHz private Wi-Fi radio via WebPA failed";
			}
			if (status) {
				LOGGER.info("S1 ACTUAL: Both 2.4 GHz & 5 GHz Private Wi-Fi SSIDs' are enabled using WebPA");
			} else {
				LOGGER.error("S1 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s2";
			status = false;
			Dut wirelessConnectedClientSettop = null;
			errorMessage = "Unable to connect to the private Wi-Fi Network Or WiFi capable devices are not available";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Connect the client to Private Wi-Fi Network and verify connection status ");
			LOGGER.info(
					"STEP 2: ACTION : Connect to the wifi using below commandsLinux :nmcli dev wifi connect <ssid> password <passwd>Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP 2: EXPECTED : Device should be connected with the Private Wi-Fi Network");
			LOGGER.info("**********************************************************************************");
			try {
				wirelessConnectedClientSettop = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = null != wirelessConnectedClientSettop;
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				LOGGER.info("S2 ACTUAL: Device has been connected with the Private Wi-Fi Network");
			} else {
				LOGGER.error("S2 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// step-3 to step-5
			BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
					wirelessConnectedClientSettop, tapEnv, BroadBandTestConstants.CONSTANT_3, testCaseId);

			stepNum = "s6";
			status = false;
			errorMessage = "Unable to login gateway admin page";
			WebDriver webDriver = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Verify the gateway admin page on Wi-Fi  client is launched and logged in successfully");
			LOGGER.info("STEP 6: ACTION : Launch the URL in Wi-Fi client : http://10.0.0.1");
			LOGGER.info(
					"STEP 6: EXPECTED : Gateway admin page on Wi-Fi client should sucessfully get launched and logged in");
			LOGGER.info("**********************************************************************************");
			try {
				status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, wirelessConnectedClientSettop);
			} catch (Exception exception) {
				LOGGER.error("Exception occurred during Gateway Admin Page login : " + exception.getMessage());
			}
			if (status) {
				webDriver = LanSideBasePage.getDriver();
				LOGGER.info(
						"STEP 6: ACTUAL : Gateway admin page is accessible from the client connected using admin/****** credential for Residential or cusadmin/****** credential for Commercial devices");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s7";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Launch the 'Connection' page from AdminUI page");
			LOGGER.info("STEP 7: ACTION : Navigate to  Connection page");
			LOGGER.info(
					"STEP 7: EXPECTED : Connection page should be successfully launched with page title as 'Gateway > At a Glance - <PARTNER>'");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to launch 'Connection' page from AdminUI page";

			status = LanSideBasePage.isPageLaunched(BroadBandWebGuiTestConstant.LINK_TEXT_CONNECTION,
					BroadbandPropertyFileHandler.getAtAGlancePageTitle());

			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL : 'Connection' page is successfully launched with page title as 'Gateway > At a Glance'");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s8";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Launch the 'Local IP Network' page from 'Connection' page");
			LOGGER.info("STEP 8: ACTION : Navigate to Gateway > Connection > Local IP Network");
			LOGGER.info(
					"STEP 8: EXPECTED : Local IP Network page should be successfully launched with page title as 'Gateway > Connection > Local IP Configuration - Public WiFi'");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to launch 'Local IP Network' page from 'Connection' page";
			lanSidePageNavigation = new LanSidePageNavigation(webDriver);
			status = lanSidePageNavigation.navigateToLocalIpConfigurationPage(device, tapEnv, webDriver);

			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL : 'Local IP Network' page is successfully launched with page title as 'Gateway > Connection > Local IP Configuration'");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s9";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION : Validate the error message when DHCP Starting address is larger than ending address");
			LOGGER.info(
					"STEP 9: ACTION : Enter the Starting address greater than ending address and click on 'Save Settings'");
			LOGGER.info(
					"STEP 9: EXPECTED : Error should be thrown as 'Beginning Address can't be larger than ending address'");
			LOGGER.info("**********************************************************************************");
			try {
				LanSideBasePage.click(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_BEGINNING_ADDRESS));
				LanSideBasePage.clear(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_BEGINNING_ADDRESS));
				LanSideBasePage.sendKeys(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_BEGINNING_ADDRESS),
						BroadBandTestConstants.STRING_10);
				LanSideBasePage.click(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_ENDING_ADDRESS));
				LanSideBasePage.clear(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_ENDING_ADDRESS));
				LanSideBasePage.sendKeys(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_ENDING_ADDRESS),
						BroadBandTestConstants.STRING_5);
				status = LanSideBasePage.verifyAndAcceptAlert(
						BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_SAVE_BUTTON,
						BroadBandTestConstants.POPUP_TTILE_ALERT,
						BroadBandTestConstants.ERROR_MESSAGE_DHCP_INVALID_ADDRESS, tapEnv);
			} catch (Exception e) {
				LOGGER.error("Exception occurred while validating the error message : " + e.getMessage());
			}
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : Error message is thrown as 'Beginning Address can't be larger than ending address' when DHCP Starting address is larger than ending address");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s10";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Validate the error message when DHCP Ending address is invalid");
			LOGGER.info("STEP 10: ACTION : Enter the ending address out of range and click on 'Save Settings'");
			LOGGER.info(
					"STEP 10: EXPECTED : Error should be thrown as 'Please enter a value less than or equal to 253'");
			LOGGER.info("**********************************************************************************");
			try {
				LanSideBasePage.click(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_ENDING_ADDRESS));
				LanSideBasePage.clear(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_ENDING_ADDRESS));
				LanSideBasePage.sendKeys(By.xpath(BroadBandWebGuiTestConstant.XPATH_FOR_DHCP_IPV4_ENDING_ADDRESS),
						BroadBandTestConstants.STRING_300);
				LanSideBasePage.click(By.xpath(BroadBandWebGuiElements.XPATH_FOR_DHCP_LEASE_TIME_AMOUNT));
				status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
						BroadBandTestConstants.ERROR_MESSAGE_DMZ_PAGE, LanSideBasePage.getText(
								By.xpath(BroadBandWebGuiElements.XPATH_FOR_DHCP_ENDING_ADDRESS_ERROR_MESSAGE)));
			} catch (Exception e) {
				LOGGER.error("Exception occurred while validating the error message : " + e.getMessage());
			}
			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL : Error message is thrown as 'Please enter a value less than or equal to 253' when DHCP Ending address is invalid");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-DHCP-NEG-5001");
	}

	/**
	 *
	 * Test Case : Verify whether the CM can act as a DHCPv4 server
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify Ethernet client is connected with Gateway</li>
	 * <li>PRE-CONDITION 2 : Verify IPv4 is assigned on the Ethernet client</li>
	 * <li>PRE-CONDITION 3 : Verify IPv6 is assigned on the Ethernet client</li>
	 * <li>PRE-CONDITION 4 : Verify Internet is accessible by using Interface IPv4
	 * on the Ethernet client</li>
	 * <li>PRE-CONDITION 5 : Verify Internet is accessible by using Interface IPv6
	 * on the Ethernet client</li>
	 * <li>PRE-CONDITION 6 : Verify the gateway Ip address</li>
	 * <li>PRE-CONDITION 7 : Get Default DHCP beginning address ,DHCP ending address
	 * ,DHCP lease time Using Webpa</li>
	 * <li>Step 1: Launch Broad band WebUI login page and verify login status</li>
	 * <li>Step 2: Launch the Local IP Network page from AdminUI page</li>
	 * <li>Step 3: Verify and Set the random DHCP IPV4 beginning address in Local IP
	 * Network Page.</li>
	 * <li>Step 4: Verify and Set the random DHCP IPV4 ending address in Local IP
	 * Network Page.</li>
	 * <li>Step 5: Verify the new DHCPv4 configuration setting are saved in the
	 * Local IP Network page.</li>
	 * <li>Step 6: Change DHCP lease time to 2 minutes of Connected client 2 using
	 * SNMP oid(1.3.6.1.4.1.1429.50.2.3.3.1.5.32)</li>
	 * <li>Step 7: Verify IPv4 is assigned on the ethernet client</li>
	 * <li>Step 8: Verify and Retrive the new DHCPv4 configuration for beginning and
	 * ending address is set using WEBPA.</li>
	 * <li>Step 9: Verify the internet connectivity in the connected lan client
	 * using ipv4 interface.</li>
	 * <li>Post-Condition : Close the Browser.</li>
	 * <li>Post-Condition : Revert the DHCP configurations to default values.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Joseph M
	 * @refactor Rakesh C N
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-WEBUI-7002")

	public void setDhcpMinMaxRange(Dut device) {

		// Variable Declaration begins
		String testCaseId = "";
		Dut deviceConnectedWithEthernet = null; // connected device to be verified
		String errorMessage = "unable to obtain ethernet client or Client configuration is invalid";
		boolean status = false;
		isBrowserOpen = false;
		isDhcpV4Configured = false;
		BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
		Map<String, String> webPaGetResponse = new HashMap<>();
		String stepNum = "s1";
		// Variable Declaration Ends

		testCaseId = "TC-RDKB-WEBUI-702";

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-7002");
		LOGGER.info("TEST DESCRIPTION: Verify whether the CM can act as a DHCPv4 server");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : Verify Ethnernet client is connected with Gateway");
		LOGGER.info("PRE-CONDITION 2 : Verify IPv4 is assigned on the Ethnernet client");
		LOGGER.info("PRE-CONDITION 3 : Verify IPv6 is assigned on the Ethnernet client");
		LOGGER.info("PRE-CONDITION 4 : Verify internet is accessible by using Interface IPv4 on the ethernet client");
		LOGGER.info("PRE-CONDITION 5 : Verify internet is accessible by using Interface IPv6 on the ethernet client");
		LOGGER.info("PRE-CONDITION 6 : Verify the gateway Ip address");
		LOGGER.info(
				"PRE-CONDITION 7 : Get Default DHCP beginning address ,DHCP ending  address ,DHCP lease time Using Webpa");
		LOGGER.info("1. Launch Broad band WebUI login page and verify login status");
		LOGGER.info("2. Launch the Local IP Network page from AdminUI page");
		LOGGER.info("3. Verify and Set the random DHCP IPV4 beginning address in Local IP Network Page.  ");
		LOGGER.info("4. Verify and Set the random DHCP IPV4 ending address in Local IP Network Page.  ");
		LOGGER.info("5. Verify the new DHCPv4 configuration setting are saved in the Local IP Network page.");
		LOGGER.info(
				"6. Change DHCP lease time to 2 minutes  of Connected client 2 using SNMP oid(1.3.6.1.4.1.1429.50.2.3.3.1.5.32)");
		LOGGER.info("7. Verify IPv4 is assigned on the ethernet client");
		LOGGER.info(
				"8. Verify and Retrive the new DHCPv4 configuration for beginning and ending address is set using WEBPA.");
		LOGGER.info("9. Verify the internet connectivity in the connected lan client using ipv4 interface.");
		LOGGER.info("POST-CONDITION : Close the open browser.");
		LOGGER.info("POST-CONDITION : Revert the DHCP configurations to default values.");
		LOGGER.info("#######################################################################################");
		;
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			deviceConnectedWithEthernet = BroadBandPreConditionUtils.executePreConditionToVerifyLanClientStatus(device,
					tapEnv);
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"PRE-CONDITION 7: DESCRIPTION :Get DHCP beginning address ,DHCP ending  address ,DHCP lease time Using Webpa");
			LOGGER.info(
					"PRE-CONDITION 7: ACTION: Execute Webpa command with Parameter :Device.DHCPv4.Server.Pool.1.MinAddress,Device.DHCPv4.Server.Pool.1.MaxAddress,Device.DHCPv4.Server.Pool.1.LeaseTime ");
			LOGGER.info("PRE-CONDITION 7: EXPECTED: Webpa get request should be successful");
			String[] webPaParametersForDhcp = {
					BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME };
			webPaGetResponse = tapEnv.executeMultipleWebPaGetCommands(device, webPaParametersForDhcp);
			broadBandResultObject = BroadBandWebPaUtils.verifyDhcpv4OutputValues(webPaGetResponse);
			status = broadBandResultObject.isStatus();
			errorMessage = broadBandResultObject.getErrorMessage();
			if (status) {
				LOGGER.info("PRE-CONDITION 7 : ACTUAL : Pre condition executed successfully");
			} else {
				LOGGER.error("PRE-CONDITION 7 : ACTUAL : Pre condition failed");
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 7 : FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			LOGGER.info("**********************************************************************************");

			verifyDhcpV4ConfigurationChangesThroughWebGui(device, testCaseId, deviceConnectedWithEthernet);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			if (isBrowserOpen) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				LOGGER.info("POST-CONDITION STEPS");
				LOGGER.info("POST-CONDITION 1: DESCRIPTION : Close the browser.");
				LOGGER.info("POST-CONDITION 1: EXPECTED : Browser should be closed successfully.");
				try {
					LanWebGuiLoginPage.closeBrowser();
					LOGGER.info("Browser closed successfully");
				} catch (Exception exception) {
					LOGGER.error("Exception occurred while closing the browser, unable to close browser.");
				}
			}
			if (isDhcpV4Configured) {
				BroadBandPostConditionUtils.executePostConditionToRevertDhcpv4ConfigurationToDefaultValues(device,
						tapEnv, webPaGetResponse, BroadBandTestConstants.STRING_CONSTANT_2);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-7002");
	}

	/**
	 * Verify ICMP Ping request ,Error message on changing firewall settings
	 * 
	 * <ol>
	 * <li>Verify connecting a WiFi client(2.4GHz or 5GHz) associated with the
	 * gateway.</li>
	 * <li>Verify the correct Ipv4 address for wifi Client.</li>
	 * <li>Verify the correct Ipv6 address for wifi Client.</li>
	 * <li>Verify the internet connectivity in the connected wifi client.</li>
	 * <li>Verify launching Broad band WebUI Lan admin login page and verify login
	 * status.</li>
	 * <li>Verify navigation to the gateway > firewall > ipv4 page and verify
	 * navigation status.</li>
	 * <li>Verify the WAN IPv4 Address of the gateway is retrieved
	 * successfully.</li>
	 * <li>Verify Firewall is set to Minimum Security.</li>
	 * <li>Verify ping to the WAN IPv4 is successful.</li>
	 * <li>Verify Firewall is selected to Medium Security.</li>
	 * <li>Verify ping to the WAN IPv4 is not successful.</li>
	 * <li>Verify Firewall is selected to Maximum Security.</li>
	 * <li>Verify ping to the WAN IPv4 is not successful.</li>
	 * <li>Verify Firewall is selected to Custom Security.</li>
	 * <li>Verify ping to the WAN IPv4 is not successful.</li>
	 * <li>Verify Firewall is reverted to Minimum Security.</li>
	 * <li>Verify ping to the WAN IPv4 is successful.</li>
	 * <li>Verify selecting CustomSecurity and selecting 'Disable enitire firewall'
	 * for Ipv4.</li>
	 * <li>Verify ping to the WAN IPv4 is successful.</li>
	 * <li>Verify navigation to the gateway > firewall > ipv6 page and verify
	 * navigation status.</li>
	 * <li>Verify selecting CustomSecurity and selecting 'Disable enitire firewall'
	 * for Ipv6.</li>
	 * <li>Verify ping to the WAN IPv4 is successful.</li>
	 * <li>Post-Condition 1: Verify closing the browser in connected client.</li>
	 * </ol>
	 * 
	 * @param device{@link Dut}
	 * 
	 * @author Prashant Mishra
	 * @refactor Athira
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true)
	@TestDetails(testUID = "TC-RDKB-FIREWALL-1006")
	public void testToVerifyIcmpPingRequestOnFirewallChange(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FIREWALL-106";
		String stepNum = "s1";
		String errorMessage = null;
		boolean status = false;
		Dut connectedClientDut = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FIREWALL-1006");
		LOGGER.info(
				"TEST DESCRIPTION: This test case is written to verify ICMP Ping request ,Error message on changing firewall settings in WiFi Client");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify connecting a WiFi client(2.4GHz or 5GHz) associated with the gateway.");
		LOGGER.info("2. Verify the correct Ipv4 address for wifi Client.");
		LOGGER.info("3. Verify the correct Ipv6 address for wifi Client.");
		LOGGER.info("4. Verify the internet connectivity in the connected wifi client.");
		LOGGER.info("5. Verify launching Broad band WebUI Lan admin login page and verify login status.");
		LOGGER.info("6. Verify navigation to the gateway > firewall > ipv4 page and verify navigation status.");
		LOGGER.info("7. Verify the WAN IPv4 Address of the gateway is retrieved successfully.");
		LOGGER.info("8. Verify Firewall is set to Minimum Security.");
		LOGGER.info("9. Verify ping to the WAN IPv4 is successful.");
		LOGGER.info("10. Verify Firewall is selected to Medium Security.");
		LOGGER.info("11. Verify ping to the WAN IPv4 is not successful.");
		LOGGER.info("12. Verify Firewall is selected to Maximum Security.");
		LOGGER.info("13. Verify ping to the WAN IPv4 is not successful.");
		LOGGER.info("14. Verify Firewall is selected to Custom Security.");
		LOGGER.info("15. Verify ping to the WAN IPv4 is not successful.");
		LOGGER.info("16. Verify Firewall is reverted to Minimum Security.");
		LOGGER.info("17. Verify ping to the WAN IPv4 is successful.");
		LOGGER.info("18. Verify selecting CustomSecurity and selecting 'Disable enitire firewall' for Ipv4.");
		LOGGER.info("19. Verify ping to the WAN IPv4 is successful.");
		LOGGER.info("20. Verify navigation to the gateway > firewall > ipv6 page and verify navigation status.");
		LOGGER.info("21. Verify selecting CustomSecurity and selecting 'Disable enitire firewall' for Ipv6.");
		LOGGER.info("22. Verify ping to the WAN IPv4 is successful.");
		LOGGER.info("Post-Condition 1: Verify closing browser in connected client.");

		LOGGER.info("#######################################################################################");

		try {
			errorMessage = "Unable to connect wifi client.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verify connecting a WiFi client(2.4GHz or 5GHz) associated with the gateway.");
			LOGGER.info("STEP 1: ACTION : Connect one of the associated wifi client to 2.4 or 5GHz wifi.");
			LOGGER.info("STEP 1: EXPECTED : One of the associated wifi client should be connected successfully.");
			LOGGER.info("**********************************************************************************");
			try {
				connectedClientDut = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = null != connectedClientDut;
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 1 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				LOGGER.info("STEP 1: ACTUAL : One of associated wifi client connected to wifi successfully.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// step-2 to step-4
			BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
					connectedClientDut, tapEnv, BroadBandTestConstants.CONSTANT_2, testCaseId);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}

		/**
		 * Common method for verify ICMP Ping request ,Error message on changing
		 * firewall settings
		 */
		methodToVerifyIcpmpPingRequestOnFirewallChangeInDifferentClients(device, connectedClientDut, tapEnv, testCaseId,
				BroadBandTestConstants.INTEGER_VALUE_5);

		LOGGER.info("ENDING TEST CASE: TC-RDKB-FIREWALL-1006");
	}

	/**
	 * Verify ICMP Ping request ,Error message on changing firewall settings
	 * 
	 * <ol>
	 * <li>Verify connecting a Ethernet client associated with the gateway.</li>
	 * <li>Verify the correct Ipv4 address for Ethernet Client.</li>
	 * <li>Verify the correct Ipv6 address for Ethernet Client.</li>
	 * <li>Verify the internet connectivity in the connected Ethernet client.</li>
	 * <li>Verify launching Broad band WebUI Lan admin login page and verify login
	 * status.</li>
	 * <li>Verify navigation to the gateway > firewall > ipv4 page and verify
	 * navigation status.</li>
	 * <li>Verify the WAN IPv4 Address of the gateway is retrieved
	 * successfully.</li>
	 * <li>Verify Firewall is set to Minimum Security.</li>
	 * <li>Verify ping to the WAN IPv4 is successful.</li>
	 * <li>Verify Firewall is selected to Medium Security.</li>
	 * <li>Verify ping to the WAN IPv4 is not successful.</li>
	 * <li>Verify Firewall is selected to Maximum Security.</li>
	 * <li>Verify ping to the WAN IPv4 is not successful.</li>
	 * <li>Verify Firewall is selected to Custom Security.</li>
	 * <li>Verify ping to the WAN IPv4 is not successful.</li>
	 * <li>Verify Firewall is reverted to Minimum Security.</li>
	 * <li>Verify ping to the WAN IPv4 is successful.</li>
	 * <li>Verify selecting CustomSecurity and selecting 'Disable enitire firewall'
	 * for Ipv4.</li>
	 * <li>Verify ping to the WAN IPv4 is successful.</li>
	 * <li>Verify navigation to the gateway > firewall > ipv6 page and verify
	 * navigation status.</li>
	 * <li>Verify selecting CustomSecurity and selecting 'Disable enitire firewall'
	 * for Ipv6.</li>
	 * <li>Verify ping to the WAN IPv4 is successful.</li>
	 * <li>Post-Condition 1: Verify closing the browser in connected client.</li>
	 * </ol>
	 * 
	 * @param device{@link Dut}
	 * 
	 * @author Prashant Mishra
	 * @refactor Athira
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true)
	@TestDetails(testUID = "TC-RDKB-FIREWALL-1007")
	public void testToVerifyIcmpPingRequestOnFirewallChangeFromEthClient(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FIREWALL-107";
		String stepNum = "s1";
		String errorMessage = null;
		boolean status = false;
		Dut connectedClientDut = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FIREWALL-1007");
		LOGGER.info(
				"TEST DESCRIPTION: This test case is written to verify ICMP Ping request ,Error message on changing firewall settings in Ethernet Client");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify connecting a Ethernet client associated with the gateway.");
		LOGGER.info("2. Verify the correct Ipv4 address for Ethernet Client.");
		LOGGER.info("3. Verify the correct Ipv6 address for Ethernet Client.");
		LOGGER.info("4. Verify the internet connectivity in the connected Ethernet client.");
		LOGGER.info("5. Verify launching Broad band WebUI Lan admin login page and verify login status.");
		LOGGER.info("6. Verify navigation to the gateway > firewall > ipv4 page and verify navigation status.");
		LOGGER.info("7. Verify the WAN IPv4 Address of the gateway is retrieved successfully.");
		LOGGER.info("8. Verify Firewall is set to Minimum Security.");
		LOGGER.info("9. Verify ping to the WAN IPv4 is successful.");
		LOGGER.info("10. Verify Firewall is selected to Medium Security.");
		LOGGER.info("11. Verify ping to the WAN IPv4 is not successful.");
		LOGGER.info("12. Verify Firewall is selected to Maximum Security.");
		LOGGER.info("13. Verify ping to the WAN IPv4 is not successful.");
		LOGGER.info("14. Verify Firewall is selected to Custom Security.");
		LOGGER.info("15. Verify ping to the WAN IPv4 is not successful.");
		LOGGER.info("16. Verify Firewall is reverted to Minimum Security.");
		LOGGER.info("17. Verify ping to the WAN IPv4 is successful.");
		LOGGER.info("18. Verify selecting CustomSecurity and selecting 'Disable enitire firewall' for Ipv4.");
		LOGGER.info("19. Verify ping to the WAN IPv4 is successful.");
		LOGGER.info("20. Verify navigation to the gateway > firewall > ipv6 page and verify navigation status.");
		LOGGER.info("21. Verify selecting CustomSecurity and selecting 'Disable enitire firewall' for Ipv6.");
		LOGGER.info("22. Verify ping to the WAN IPv4 is successful.");
		LOGGER.info("Post-Condition 1: Verify closing browser in connected client.");

		LOGGER.info("#######################################################################################");

		try {
			errorMessage = "Unable to obtain Ethernet client.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify obtaining a Ethernet client associated with the gateway.");
			LOGGER.info("STEP 1: ACTION : Obtain one of the associated Ethernet client.");
			LOGGER.info("STEP 1: EXPECTED : One of the associated Ethernet client should be obtained successfully.");
			LOGGER.info("**********************************************************************************");
			try {
				connectedClientDut = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = null != connectedClientDut;
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : One of associated Ethernet client obtained successfully.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// step-2 to step-4
			BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
					connectedClientDut, tapEnv, BroadBandTestConstants.CONSTANT_2, testCaseId);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}

		/** ICMP Ping request ,Error message on changing firewall settings */
		methodToVerifyIcpmpPingRequestOnFirewallChangeInDifferentClients(device, connectedClientDut, tapEnv, testCaseId,
				BroadBandTestConstants.INTEGER_VALUE_5);

		LOGGER.info("ENDING TEST CASE: TC-RDKB-FIREWALL-1007");
	}

	/**
	 * Common method to verify ICMP Ping request ,Error message on changing firewall
	 * settings in connected client(Ethernet & WiFI both client type)
	 * 
	 * @param device                Settop instance
	 * @param connectedClientSettop Connected client device
	 * @param tapEnv                ECatsTapApi instance
	 * @param testCaseId            Test case id for which method is being called
	 * @param stepNumber            Starting step number
	 * @param clientType            Client connection type
	 * 
	 * @author Prashant Mishra
	 * @refactor Athira
	 */
	public static void methodToVerifyIcpmpPingRequestOnFirewallChangeInDifferentClients(Dut device,
			Dut connectedClientDevice, AutomaticsTapApi tapEnv, String testCaseId, int stepNumber) {
		// Variable declaration starts
		String stepNum = null;
		String errorMessage = null;
		String wanIpAddress = null;
		boolean status = false;
		boolean isBrowserOpen = false;
		boolean pingResult = false;
		WebDriver webDriver = null;
		// Variable declaration ends

		try {
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to launch ADMIN UI page.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify launching Broad band WebUI Lan admin login page and verify login status.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Get LAN IP address using webpa and launch url: http://<Lan Ip address> and login with proper credentials.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Admin login page should be launched and login should be successful.");
			LOGGER.info("**********************************************************************************");
			isBrowserOpen = status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, connectedClientDevice);
			webDriver = LanWebGuiLoginPage.getDriver();
			LanSidePageNavigation lanSidePageNavigation = new LanSidePageNavigation(webDriver);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Broad band WebUI Lan admin login page launched and logged in successfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to verify navigation status on gateway > firewall > ipv4 page.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify navigation to the gateway > firewall > ipv4 page and verify navigation status.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Navigate to gateway > firewall > ipv4 link and verify page title.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Navigation should be successful and it should display the gateway > firewall > ipv4 page.");
			LOGGER.info("**********************************************************************************");
			try {
				if (BroadBandCommonPage.navigateToFirewall(webDriver, device)) {
					errorMessage = "Unable to navigate to Firewall IPv4 page from the 'Firewall' Menu";
					status = LanWebGuiLoginPage.isFireWallPageLaunchedForPartners(device, tapEnv,
							BroadBandWebGuiTestConstant.LINK_TEXT_IPV4, BroadBandTestConstants.FIREWALL_IPV4);
				}
			} catch (Exception e) {
				LOGGER.error("Exception occured while navigating to Firewall IPV4 Page :" + e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : IPv4 Firewall page is displayed having page title as 'Gateway > Firewall> IPv4'");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to retrieve WAN IP Address of the gateway using WebPA/dmcli.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify the WAN IPv4 Address of the gateway is retrieved successfully.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute WebPA get command on parameter 'Device.DeviceInfo.X_COMCAST-COM_WAN_IP'.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : WAN IP Address of the gateway should be retieved successfully.");
			LOGGER.info("**********************************************************************************");
			wanIpAddress = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV4);
			status = CommonMethods.isNotNull(wanIpAddress);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL : WAN IPv4 Address of the gateway is retrieved successfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to set Firewall to Minimum Security (Low) from GUI.";
			status = false;
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : Verify Firewall is selected to Minimum Security (Low).");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Click on Minimum Security (Low) radio button and click on save settings button.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Minimum Security (Low) firewall should be saved.");
			LOGGER.info("#######################################################################################");

			status = BroadBandCommonPage.configureFirewallSetting(tapEnv, connectedClientDevice, webDriver,
					BroadBandWebGuiElements.ELEMENT_ID_MINIMUM_FIREWALL,
					BroadBandTestConstants.DEFAULT_IPV4_FIREWALL_SECURITY, BroadBandTestConstants.String_CONSTANT_IPV4);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Firewall is selected to Minimum Security (Low) successfully,");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to validate the ping to WAN IP.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify ping to the WAN IPv4 is successful.");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute ping command ping <WAN IPv4>.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ping should be successful.");
			LOGGER.info("**********************************************************************************");
			
			status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientDevice, tapEnv,
					wanIpAddress,BroadBandTestConstants.STRING_VALUE_FIVE);
			
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Verified ping request TO WAN IP suceessfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s" + stepNumber;
			errorMessage = "Unable to set Firewall to Medium Security (Low) from GUI.";
			status = false;
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify Firewall is selected to Medium Security");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Click on Typical Security (Medium) radio button and click on save settings button.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Typical Security (Medium) firewall should be saved.");
			LOGGER.info("#######################################################################################");

			status = BroadBandCommonPage.configureFirewallSetting(tapEnv, connectedClientDevice, webDriver,
					BroadBandWebGuiElements.ELEMENT_ID_MEDIUM_FIREWALL,
					BroadBandTestConstants.TYPICAL_FIREWALL_SECURITY, BroadBandTestConstants.String_CONSTANT_IPV4);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Firewall is selected to Minimum Security (Low) successfully,");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to validate the ping to WAN IP in medium security firewall.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify ping to the WAN IPv4 is not successful.");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute ping command ping <WAN IPv4>.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ping should not be successful.");
			LOGGER.info("**********************************************************************************");
			status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientDevice, tapEnv,
					wanIpAddress,BroadBandTestConstants.STRING_VALUE_FIVE);
			
			status = !pingResult;
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Verified ping request TO WAN IP suceessfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to set Firewall to Maximum Security from GUI.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify Firewall is selected to Maximum Security.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Click on Maximum Security radio button and click on save settings.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED  Security firewall should be saved.: ");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonPage.configureFirewallSetting(tapEnv, connectedClientDevice, webDriver,
					BroadBandWebGuiElements.ELEMENT_ID_MAXIMUM_FIREWALL,
					BroadBandTestConstants.MAXIMUM_FIREWALL_SECURITY, BroadBandTestConstants.String_CONSTANT_IPV4);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Firewall is selected to Maximum Security successfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to validate the ping to WAN IP in maximum security firewall.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify ping to the WAN IPv4 is not successful.");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute ping command ping <WAN IPv4>.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ping should not be successful.");
			LOGGER.info("**********************************************************************************");
			status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientDevice, tapEnv,
					wanIpAddress,BroadBandTestConstants.STRING_VALUE_FIVE);
			
			status = !pingResult;
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Verified ping request TO WAN IP suceessfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to set Firewall to Custom Security from GUI.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify Firewall is selected to Custom Security.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Click on Custom Security radio button and click on save settings.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED  Custom Security firewall should be saved. ");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonPage.configureCustomFirewallSetting(tapEnv, device, webDriver,
					BroadBandWebGuiElements.XPATH_CUSTOM_FIREWALL_LINK,
					BroadBandTestConstants.CUSTOM_IPV4_FIREWALL_SECURITY, BroadBandTestConstants.String_CONSTANT_IPV4,
					BroadBandWebGuiElements.XPATH_DISABLE_ENTIRE_FIREWALL);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Firewall is selected to Custom Security successfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to validate the ping to WAN IP in custom security firewall.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify ping to the WAN IPv4 is not successful.");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute ping command ping <WAN IPv4>.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ping should not be successful.");
			LOGGER.info("**********************************************************************************");
			status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientDevice, tapEnv,
					wanIpAddress,BroadBandTestConstants.STRING_VALUE_FIVE);
			
			status = !pingResult;
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Verified ping request TO WAN IP suceessfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to revert Firewall to Minimum Security from GUI.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify Firewall is reverted to Minimum Security.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Click on Minimum Security radio button and click on save settings.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED  Minimum Security firewall should be reverted.: ");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonPage.configureFirewallSetting(tapEnv, connectedClientDevice, webDriver,
					BroadBandWebGuiElements.ELEMENT_ID_MINIMUM_FIREWALL,
					BroadBandTestConstants.DEFAULT_IPV4_FIREWALL_SECURITY, BroadBandTestConstants.String_CONSTANT_IPV4);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Firewall is reverted to Minimum Security successfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to validate the ping to WAN IP in minimum security firewall.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify ping to the WAN IPv4 is successful.");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute ping command ping <WAN IPv4>.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ping should be successful.");
			LOGGER.info("**********************************************************************************");
			status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientDevice, tapEnv,
					wanIpAddress,BroadBandTestConstants.STRING_VALUE_FIVE);
			
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Verified ping request TO WAN IP suceessfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to set firewall settings for disable entire firewall.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify selecting CustomSecurity and selecting 'Disable enitire firewall' for Ipv4.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Click on Custom Security link and click on Disable enitre firewall check Box & click on cancel button");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Disable entire firewall should be checked in and should be cancelled successfully.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonPage.configureCustomFirewallSetting(tapEnv, device, webDriver,
					BroadBandWebGuiElements.XPATH_CUSTOM_FIREWALL_LINK,
					BroadBandTestConstants.CUSTOM_IPV4_FIREWALL_SECURITY, BroadBandTestConstants.String_CONSTANT_IPV6,
					BroadBandWebGuiElements.XPATH_DISABLE_ENTIRE_FIREWALL);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Selecting 'Disable enitire firewall' and cancelling verified for Ipv4.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to validate the ping to WAN IP.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify ping to the WAN IPv4 is successful.");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute ping command ping <WAN IPv4>.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ping should be successful.");
			LOGGER.info("**********************************************************************************");
			status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientDevice, tapEnv,
					wanIpAddress,BroadBandTestConstants.STRING_VALUE_FIVE);
			
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Verified ping request TO WAN IP suceessfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to verify navigation status on gateway > firewall > ipv6 page.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify navigation to the gateway > firewall > ipv6 page and verify navigation status.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Navigate to gateway > firewall > ipv6 link and verify page title.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Navigation should be successful and it should display the gateway > firewall > ipv6 page.");
			LOGGER.info("**********************************************************************************");

			try {
				errorMessage = "Unable to navigate to Firewall IPv6 page from the 'Firewall' Menu";

				status = LanSideBasePage.isPageLaunched(BroadBandWebGuiTestConstant.LINK_TEXT_IPV6,
						BroadbandPropertyFileHandler.getFireWallIpv6PageTitle());
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : NAVIGATION SUCCESSFUL FOR GATEWAY > FIREWALL > IPV6 PAGE");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
			} catch (Exception e) {
				LOGGER.error("Exception occured while navigating to Firewall IPV4 Page :" + e.getMessage());
			}

			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to set firewall settings for disable entire firewall.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify selecting CustomSecurity and selecting 'Disable enitire firewall' for Ipv6.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Click on Custom Security link and click on Disable enitre firewall check Box & click on cancel button");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Disable entire firewall should be checked in and should be cancelled successfully.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonPage.configureCustomFirewallSetting(tapEnv, device, webDriver,
					BroadBandWebGuiElements.XPATH_CUSTOM_FIREWALL_LINK,
					BroadBandTestConstants.CUSTOM_IPV4_FIREWALL_SECURITY, BroadBandTestConstants.String_CONSTANT_IPV4,
					BroadBandWebGuiElements.XPATH_DISABLE_ENTIRE_FIREWALL);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Selecting 'Disable enitire firewall' and cancelling verified for Ipv6.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNumber++;
			stepNum = "s" + stepNumber;
			errorMessage = "Unable to validate the ping to WAN IP.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify ping to the WAN IPv4 is successful.");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute ping command ping <WAN IPv4>.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ping should be successful.");
			LOGGER.info("**********************************************************************************");
			status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientDevice, tapEnv,
					wanIpAddress,BroadBandTestConstants.STRING_VALUE_FIVE);
			
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Verified ping request TO WAN IP suceessfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################## STARTING POST-CONFIGURATIONS ##################");
			LOGGER.info("Post-Condition Steps: ");
			int postConditionNum = 0;

			if (isBrowserOpen) {
				postConditionNum++;
				status = false;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postConditionNum
						+ ": DESCRIPTION : Verify the opened browser is closed successfully in connected client.");
				LOGGER.info("POST-CONDITION " + postConditionNum + ": ACTION : Close the browser.");
				LOGGER.info(
						"POST-CONDITION " + postConditionNum + ": EXPECTED : Browser should be closed successfully.");
				LOGGER.info("#######################################################################################");
				try {
					LanWebGuiLoginPage.closeBrowser();
					LOGGER.info("POST-CONDITION " + postConditionNum
							+ ": ACTUAL : Opened Browser closed successfully in connected client.");
				} catch (Exception exception) {
					errorMessage = "Exception occurred while closing the browser, Unable to close browser."
							+ exception.getMessage();
					LOGGER.error("POST-CONDITION " + postConditionNum + ": ACTUAL :" + errorMessage);
				}
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
	}

	/**
	 * Test to verify Providing option in local UI to add/remove Reserved IP binding
	 * for offline devices from lan page
	 * <ol>
	 * <li>Get wifi connected client and connect to 2.4Ghz or 5Ghz SSID</li>
	 * <li>Login to LAN UI</li>
	 * <li>Navigate to connected devices page</li>
	 * <li>Verify wifi connected device is present in online devices</li>
	 * <li>Verify add or remove Reserved IP binding is present in edit option for
	 * online connected device</li>
	 * <li>Disconnect Wifi Radio SSID</li>
	 * <li>Get wifi connected client and connect to 2.4Ghz or 5Ghz SSID</li>
	 * <li>Login to LAN UI</li>
	 * <li>Navigate to connected devices page</li>
	 * <li>Verify wifi disconnected device is present in offline devices</li>
	 * <li>Verify add or remove Reserved IP binding is present in edit option for
	 * offline connected device</li>
	 * </ol>
	 * 
	 * @author Betel Costrow
	 * @Refactor Sruthi Santhosh
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-WEBUI-8026")
	public void testToVerifyReservedIpOptionFromLan(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WEBUI-026";
		String stepNum = "s1";
		String errorMessage = null;
		Dut wifiClient = null;
		Dut clientSettop = null;
		boolean status = false;
		String partnerId = null;
		int count = BroadBandTestConstants.CONSTANT_0;
		int rowNumber = BroadBandTestConstants.CONSTANT_0;
		List<BroadBandConnectedClientInfo> listOfActiveDevices = null;
		List<BroadBandConnectedClientInfo> listOfInactiveDevices = null;
		String macAddress = null;
		String hostName = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-8026");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify Providing option in local UI to add/remove Reserved IP binding for offline devices from lan page");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Get wifi connected client and connect to 2.4Ghz or 5Ghz SSID");
		LOGGER.info("2. Get wifi connected client and connect to 2.4Ghz or 5Ghz SSID");
		LOGGER.info("3. Login to LAN UI ");
		LOGGER.info("4. Navigate to connected devices page");
		LOGGER.info("5. Verify wifi connected device is present in online devices");
		LOGGER.info(
				"6. Verify add or remove Reserved IP binding is present in edit option for online connected device");
		LOGGER.info("7. Disconnect Wifi Radio SSID ");
		LOGGER.info("8. Login to LAN UI ");
		LOGGER.info("9. Navigate to connected devices page");
		LOGGER.info("10. Verify wifi disconnected device is present in  offline devices");
		LOGGER.info(
				"11. Verify add or remove Reserved IP binding is present in edit option for offline connected device");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("############################# STARTING PRE-CONFIGURATIONS #############################");
			/** PRE-CONDITION 1: To retrieve partner id of the device */
			partnerId = BroadBandPreConditionUtils.executePreConditionToCheckPartnerId(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

			errorMessage = "Failed to get and connect 2.4 or 5 Ghz Wifi capable client";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Get wifi connected client and connect to 2.4Ghz or 5Ghz SSID");
			LOGGER.info(
					"STEP 1: ACTION : 1. Get windows client from list of connected devices in inventory2. Get WiFi SSID and passphrase and check if visible from client3. Connect and verify connected state");
			LOGGER.info("STEP 1: EXPECTED : Successfully connected Client to 2.4 or 5 Ghz SSID");
			LOGGER.info("**********************************************************************************");

			wifiClient = BroadBandConnectedClientUtils
					.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			if (wifiClient != null) {
				macAddress = ((Device) wifiClient).getConnectedDeviceInfo().getWifiMacAddress();
				if (CommonMethods.isNotNull(macAddress)) {
					hostName = BroadBandCommonUtils.getConnectedClientDetailsUsingWebpa(device, tapEnv, macAddress,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_HOST_HOSTNAME);
					status = CommonMethods.isNotNull(hostName);
				}
			}

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully connected Client to 2.4 or 5 Ghz SSID");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Failed to get and connect 2.4 or 5 Ghz Wifi capable client";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Get wifi connected client and connect to 2.4Ghz or 5Ghz SSID");
			LOGGER.info(
					"STEP 2: ACTION : 1. Get windows client from list of connected devices in inventory2. Get WiFi SSID and passphrase and check if visible from client3. Connect and verify connected state");
			LOGGER.info("STEP 2: EXPECTED : Successfully connected Client to 2.4 or 5 Ghz SSID");
			LOGGER.info("**********************************************************************************");

			try {
				clientSettop = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(device, tapEnv,
						wifiClient, BroadBandTestConstants.BAND_2_4GHZ);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = clientSettop != null;

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully Gets another client and connected Client to 2.4 SSID");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Failed to login to LAN UI in WiFi client";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Login to LAN UI ");
			LOGGER.info(
					"STEP 3: ACTION : 1. Launch 10.0.0.1 or 10.1.10.1 in browser of WiFi client2. Login with admin/cusadmin user credentials");
			LOGGER.info("STEP 3: EXPECTED : Login should be successful");
			LOGGER.info("**********************************************************************************");

			status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, wifiClient);
			driver = LanWebGuiLoginPage.getDriver();

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Logged into LAN page");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			errorMessage = "Failed to navigate to Connected Devices -> Devices page";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Navigate to connected devices page");
			LOGGER.info("STEP 4: ACTION : Naviagate to At a glance -> Connected Devices -> Devices");
			LOGGER.info("STEP 4: EXPECTED : Successfully launched Devices page");
			LOGGER.info("**********************************************************************************");

			LanSidePageNavigation lanSidePageNavigation = new LanSidePageNavigation(driver);
			status = lanSidePageNavigation.navigateToConnectedDevicesPage(device, tapEnv, driver);

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Successfully launched Devices page");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "Wifi connected device is not present in online devices";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify wifi connected device is present in online devices");
			LOGGER.info("STEP 5: ACTION : check wifi connected device mac is present there");
			LOGGER.info("STEP 5: EXPECTED : wifi connected device mac should present ");
			LOGGER.info("**********************************************************************************");

			listOfActiveDevices = BroadBandCommonPage.getDetailsOfActiveDevicesFromGui(driver);
			if (listOfActiveDevices != null) {
				for (BroadBandConnectedClientInfo broadBandConnectedClientInfo : listOfActiveDevices) {
					status = broadBandConnectedClientInfo.getHostName().equalsIgnoreCase(hostName);
					count++;
					if (status == true) {
						LOGGER.info("Wifi connected client is present in rownumber " + count);
						LOGGER.info(hostName
								+ " is the client taken in step1.Checking the same client Host name is available in online devices section "
								+ broadBandConnectedClientInfo.getHostName());
						break;
					}
				}
				rowNumber = count;
			}

			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Successfully verified wifi connected client is present in online devices");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "Add or remove reserved ip address field is grayed out";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Verify add or remove Reserved IP binding is present in edit option for online connected device");
			LOGGER.info(
					"STEP 6: ACTION : 1.click on edit option in wifi connected client2.change configuration Reserved IP3.Verify reserved ip address is not grayed out");
			LOGGER.info("STEP 6: EXPECTED : Add or remove reserved ip address option should available ");
			LOGGER.info("**********************************************************************************");

			try {
				driver.findElement(By.xpath(BroadBandWebGuiTestConstant.DYNAMIC_XPATH_FOR_EDIT_BTN
						.replace(BroadBandTestConstants.TR181_NODE_REF, String.valueOf(rowNumber)))).click();
				LOGGER.info("Navigated to edit page");
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				driver.findElement(By.xpath(BroadBandWebGuiTestConstant.DYNAMIC_XPATH_RESERVEDIP_RADIO
						.replace(BroadBandTestConstants.TR181_NODE_REF, String.valueOf(rowNumber)))).click();
				status = driver
						.findElement(By.xpath(BroadBandWebGuiTestConstant.DYNAMIC_XPATH_RESERVEDIP_ADDRESS
								.replace(BroadBandTestConstants.TR181_NODE_REF, String.valueOf(rowNumber))))
						.isEnabled();
				LOGGER.info("Result of checking Reserved Ip is enabled:" + status);
			} catch (Exception e) {
				LOGGER.info("Exeception occured while " + e.getMessage());
			}

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Successfully verified Reserved ip is enabled for online devices");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s7";
			errorMessage = "Wifi SSID is not disconnected";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Disconnect Wifi Radio SSID ");
			LOGGER.info("STEP 7: ACTION : Disconnect wifi radio SSID in connected client");
			LOGGER.info("STEP 7: EXPECTED : Wifi radio SSID should be disconnected successfully");
			LOGGER.info("**********************************************************************************");

			BroadBandResultObject bandResultObject = BroadBandConnectedClientUtils.disconnectCnnClientFromSsid(tapEnv,
					device, wifiClient);
			status = bandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Successfully disconnected wifi on client device");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			errorMessage = "Failed to login to LAN UI in WiFi client";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Login to LAN UI ");
			LOGGER.info(
					"STEP 8: ACTION : 1. Launch 10.0.0.1 or 10.1.10.1 in browser of WiFi client2. Login with admin/cusadmin user credentials");
			LOGGER.info("STEP 8: EXPECTED : Login should be successful");
			LOGGER.info("**********************************************************************************");

			status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, clientSettop);
			driver = LanWebGuiLoginPage.getDriver();

			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Logged into LAN page");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}

			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			errorMessage = "Failed to navigate to Connected Devices -> Devices page";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Navigate to connected devices page");
			LOGGER.info("STEP 9: ACTION : Naviagate to At a glance -> Connected Devices -> Devices");
			LOGGER.info("STEP 9: EXPECTED : Successfully launched Devices page");
			LOGGER.info("**********************************************************************************");

			status = lanSidePageNavigation.navigateToConnectedDevicesPage(device, tapEnv, driver);

			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Successfully launched Devices page");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}

			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			errorMessage = "Wifi disconnected device is not present in online devices";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify wifi disconnected device is present in  offline devices");
			LOGGER.info("STEP 10: ACTION : check wifi disconnected device mac is present there");
			LOGGER.info("STEP 10: EXPECTED : wifi disconnected device mac should present ");
			LOGGER.info("**********************************************************************************");

			listOfInactiveDevices = BroadBandCommonPage.getDetailsOfInActiveDevicesFromGui(driver);
			if (listOfInactiveDevices != null) {
				for (BroadBandConnectedClientInfo broadBandConnectedClientInfo : listOfInactiveDevices) {
					status = hostName.equalsIgnoreCase(broadBandConnectedClientInfo.getHostName());
					if (status == true) {
						LOGGER.info(hostName
								+ " is the client taken in step1.Checking the same client Host name is available in offline devices section "
								+ broadBandConnectedClientInfo.getHostName());
						break;
					}
				}
			}

			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL : Successfully verified after disconnected wifi device is present in offline devices");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s11";
			errorMessage = "Add or remove reserved ip address field is grayed out";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 11: DESCRIPTION : Verify add or remove Reserved IP binding is present in edit option for offline connected device");
			LOGGER.info(
					"STEP 11: ACTION : 1.click on edit option in wifi connected client2.change configuration Reserved IP3.Verify reserved ip address is not grayed out");
			LOGGER.info("STEP 11: EXPECTED : Add or remove reserved ip address option should available ");
			LOGGER.info("**********************************************************************************");

			try {
				driver.findElement(By.xpath(BroadBandWebGuiTestConstant.DYNAMIC_XPATH_FOR_EDIT_BTN
						.replace(BroadBandTestConstants.TR181_NODE_REF, String.valueOf(rowNumber)))).click();
				LOGGER.info("Navigated to edit page");
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				driver.findElement(By.xpath(BroadBandWebGuiTestConstant.DYNAMIC_XPATH_RESERVEDIP_RADIO
						.replace(BroadBandTestConstants.TR181_NODE_REF, String.valueOf(rowNumber)))).click();
				status = driver
						.findElement(By.xpath(BroadBandWebGuiTestConstant.DYNAMIC_XPATH_RESERVEDIP_ADDRESS
								.replace(BroadBandTestConstants.TR181_NODE_REF, String.valueOf(rowNumber))))
						.isEnabled();
				LOGGER.info("Result of checking Reserved Ip is enabled:" + status);
			} catch (Exception e) {
				LOGGER.info("Exeception occured while " + e.getMessage());
			}

			if (status) {
				LOGGER.info("STEP 11: ACTUAL : Successfully verified Reserved ip is enabled for offline devices");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}

			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(driver, tapEnv, device, testCaseId, stepNum, status,
					errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-8026");
	}

}
