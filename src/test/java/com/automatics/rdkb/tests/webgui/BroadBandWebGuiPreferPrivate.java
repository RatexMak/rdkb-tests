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
package com.automatics.rdkb.tests.webgui;

import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWifiBaseTest;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.webui.page.LanSideBasePage;
import com.automatics.rdkb.webui.page.LanSidePageNavigation;
import com.automatics.rdkb.webui.page.LanWebGuiLoginPage;
import com.automatics.rdkb.webui.utils.BroadBandWebUiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.CommonMethods;

/**
 * Test class for Validating prefer private wifi functionality testcases through GUI
 * 
 * @author Joseph Maduram
 * @refactor Rakesh C N
 */

public class BroadBandWebGuiPreferPrivate extends BroadBandWifiBaseTest {
	
    /**
     * Prefer Private functionality should be applicable for PublicWiFi open and secure hotspot SSIDs
     * <ol>
     * <li>Connect to the Connected client having 2.4GHZ wifi Capability</li>
     * <li>Check if the wirless connected client has an IPv4 address from the gateway.</li>
     * <li>Verify whether you have connectivity in the connected client using IPV4 address.</li>
     * <li>Check if the wirless connected client has an IPv6 address from the gateway.</li>
     * <li>Verify whether you have connectivity in the connected client using IPV6 address.</li>
     * <li>Launch Broad band LAN UI login page and verify login status</li>
     * <li>Launch the Connected Devices page from AdminUI page</li>
     * <li>Verify whether Prefer Private option is Checked(enabled) from Local GUI page</li>
     * <li>Verify the PreferPrivate option is enabled via WebPA</li>
     * <li>Execute webPA Get command on Device.WiFi.AccessPoint.10003.X_COMCAST-COM_MAC_FilteringMode</li>
     * <li>Execute webPA Get command on Device.WiFi.AccessPoint.10103.X_COMCAST-COM_MAC_FilteringMode</li>
     * <li>Verify whether Prefer Private option is UnChecked(disabled) from Local GUI page</li>
     * <li>Verify the PreferPrivate option is disabled via WebPA</li>
     * <li>Execute webPA Get command on Device.WiFi.AccessPoint.10003.X_COMCAST-COM_MAC_FilteringMode</li>
     * <li>Execute webPA Get command on Device.WiFi.AccessPoint.10103.X_COMCAST-COM_MAC_FilteringMode</li>
     * </ol>
     * 
     * @author Joseph Maduram
     * @refactor Rakesh C N
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-WIFI-PREFERPRIVATE-1002")
    public void preferPrivate(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI-PREFERPRIVATE-102";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	String preferPrivateStatus = "";// String variable to store the Prefer Private Status.
	WebDriver driver = null;
	Dut connectedDeviceActivated = null;// Dut object to store client device
	WebDriver webDriver = null;
	// Variable Declation Ends

	testCaseId = "TC-RDKB-WIFI-PREFERPRIVATE-102";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-PREFERPRIVATE-1002");
	LOGGER.info(
		"TEST DESCRIPTION: Prefer Private functionality should be applicable for PublicWiFi open and secure hotspot SSIDs");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Connect to the Connected client having  2.4GHZ wifi Capability");
	LOGGER.info("2. Check if the wirless connected client has an IPv4 address from the gateway.");
	LOGGER.info("3. Verify whether you have connectivity in the connected client using IPV4 address.");
	LOGGER.info("4. Check if the wirless connected client has an IPv6 address from the gateway.");
	LOGGER.info("5. Verify whether you have connectivity in the connected client using IPV6 address.");
	LOGGER.info("6. Launch Broad band LAN UI login page and verify login status");
	LOGGER.info("7. Launch the Connected Devices page from AdminUI page");
	LOGGER.info("8. Verify whether Prefer Private option is Checked(enabled) from Local GUI page");
	LOGGER.info("9. Verify the PreferPrivate option is enabled via WebPA");
	LOGGER.info("10. Execute webPA Get command  on Device.WiFi.AccessPoint.10003.X_COMCAST-COM_MAC_FilteringMode ");
	LOGGER.info("11. Execute webPA Get command  on Device.WiFi.AccessPoint.10103.X_COMCAST-COM_MAC_FilteringMode ");
	LOGGER.info("12. Verify whether Prefer Private option is UnChecked(disabled) from Local GUI page");
	LOGGER.info("13. Verify the PreferPrivate option is disabled via WebPA");
	LOGGER.info("14. Execute webPA Get command  on Device.WiFi.AccessPoint.10003.X_COMCAST-COM_MAC_FilteringMode ");
	LOGGER.info("15. Execute webPA Get command  on Device.WiFi.AccessPoint.10103.X_COMCAST-COM_MAC_FilteringMode ");
	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "S1";
	    errorMessage = "Unable to connect to 2.4GHZ wifi";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Connect to the Connected client having  2.4GHZ wifi Capability");
	    LOGGER.info("STEP 1: ACTION : connect to the client having wifi capablilty as 2.4 ghz/Dual band");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Device should be able to connect with the connected client having 2.4GHZ wifi Capability");
	    LOGGER.info("**********************************************************************************");
	    connectedDeviceActivated = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    status = (null != connectedDeviceActivated);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Device is connected with the connected client having 2.4Ghz Wifi Capability");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    // verifying ipv4 and ipv6 and connectivity
	    BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, connectedDeviceActivated, 2);

	    stepNum = "S6";
	    errorMessage = "Unable to Login to LanGUI page using Admin credential";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Launch Broad band LAN UI login page and verify login status");
	    LOGGER.info(
		    "STEP 6: ACTION : Launch the below URL format in browserhttps://10.0.0.1/LOGIN CREDENTIALS :  username: 'admin' Password: 'password'");
	    LOGGER.info("STEP 6: EXPECTED : AdminUI page should be launched and login should be successful");
	    LOGGER.info("**********************************************************************************");
	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, connectedDeviceActivated);
	    webDriver = LanWebGuiLoginPage.getDriver();
	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Launch Broad band LAN UI login page and verify login status is successful");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S7";
	    errorMessage = "Connected Devices page is not  displayed having page title as \"Connected Devices >Devices\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Launch the Connected Devices page from public page");
	    LOGGER.info(
		    "STEP 7: ACTION : Navigate to Connected Devices page by clicking on \"Connected Decvices\" Menu  and select the submenu \"Devices\" ");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Connected Devices page should get displayed having page title as \"Connected Devices >Devices\"");
	    LOGGER.info("**********************************************************************************");
	    try {
		LanSidePageNavigation lanSidePageNavigation = new LanSidePageNavigation(webDriver);
		status=lanSidePageNavigation.navigateToConnectedDevicesPage(device, tapEnv, webDriver);
	    } catch (Exception exception) {
		LOGGER.error("Exception occurred while navigating to connected devices page in LAN  GUI Page: "
			+ exception.getMessage());
	    }
	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : Connected Devices Page is displayed having Page Title as Connected Devices >Devices");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S8";
	    errorMessage = "Prefer Private option is not Checked(Enabled) by default in Local GUI page.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify whether Prefer Private option is Checked(enabled) from Local GUI page");
	    LOGGER.info("STEP 8: ACTION : Verify the state of Prefer Private option from Local GUI page.");
	    LOGGER.info(
		    "STEP 8: EXPECTED : Prefer Private option should be Checked(Enabled) by default in Local GUI page.");
	    LOGGER.info("**********************************************************************************");
	    try {
		driver = LanSideBasePage.getDriver();
		status = BroadBandWebUiUtils.isPreferredPrivateCheckBoxEnabled(driver,
			BroadBandTestConstants.BOOLEAN_VALUE_TRUE);
	    } catch (Exception exception) {
		LOGGER.error("Exception occurred while clicking private wifi" + exception.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : prefer private option is in enabled state");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S9";
	    errorMessage = "Unable to retrieve the status of Prefer Private 'Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate' as true using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9:DESCRIPTION : Verify the PreferPrivate option is enabled via WebPA");
	    LOGGER.info("STEP 9:ACTION : Execute the WEBPA Command To check the prefer private option is enabled.");
	    LOGGER.info(
		    "STEP 9:EXPECTED : Prefer Private Option Should be Enabled and The WEBPA Command should return True.");
	    LOGGER.info("**********************************************************************************");
	    preferPrivateStatus = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS);
	    LOGGER.info("Prefer Private status retrieved using WebPa = " + preferPrivateStatus);
	    status = CommonMethods.isNotNull(preferPrivateStatus)
		    && preferPrivateStatus.equalsIgnoreCase(BroadBandTestConstants.TRUE);
	    errorMessage = errorMessage + ":Actual=" + preferPrivateStatus + " " + "Expected="
		    + BroadBandTestConstants.TRUE;
	    if (status) {
		LOGGER.info(
			"S9 ACTUAL: Successfully retrieved  the status of Prefer Private 'Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate' as true using WebPA command.");
	    } else {
		LOGGER.error("S9 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S10";
	    errorMessage = "The Filtering Mode for Public Wifi 2.4Ghz(AccessPoint.10003) is not denied after making the preferred Private option as enabled";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Execute webPA Get command  on Device.WiFi.AccessPoint.10003.X_COMCAST-COM_MAC_FilteringMode ");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute the command:curl -X GET -H \"Authorization: Bearer <SAT token>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i <WEBPA_SERVER_URL><CM_MAC>/config?names=Device.WiFi.AccessPoint.10003.X_COMCAST-COM_MAC_FilteringMode");
	    LOGGER.info(
		    "STEP 10: EXPECTED : As the prefer private is true, the Filtering Mode for Public Wifi 2.4 (AccessPoint.10003) should be deny.");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("waiting for 1 mins to reflect");
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_COMCAST_COM_MAC_FILTERINGMODE);
	    LOGGER.info("value retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.STRING_DENY);
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : As the prefer private is true, the Filtering Mode for Public Wifi 2.4 (AccessPoint.10003) is denied.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S11";
	    errorMessage = "The Filtering Mode for Public Wifi 5ghz (AccessPoint.10103) is not denied after making the preferred Private option as enabled";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Execute webPA Get command  on Device.WiFi.AccessPoint.10103.X_COMCAST-COM_MAC_FilteringMode ");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the command:curl -X GET -H \"Authorization: Bearer <SAT token>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i <WEBPA_SERVER_URL><CM_MAC>/config?names=Device.WiFi.AccessPoint.10103.X_COMCAST-COM_MAC_FilteringMode");
	    LOGGER.info(
		    "STEP 11: EXPECTED : As the prefer private is true, the Filtering Mode for Public Wifi 5Ghz (AccessPoint.10103) should be deny.");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_COMCAST_COM_MAC_FILTERINGMODE);
	    LOGGER.info("value retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.STRING_DENY);
	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : As the prefer private is true, the Filtering Mode for Public Wifi 5 (AccessPoint.10103) is denied.");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S12";
	    errorMessage = "Prefer private connection is not in disabled state";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify whether Prefer Private option is unChecked(disabled) from Local GUI page");
	    LOGGER.info("STEP 12: ACTION : Verify the state of Prefer Private option from Local GUI page.");
	    LOGGER.info(
		    "STEP 12: EXPECTED : Prefer Private option should be unChecked(disabled) by default in Local GUI page.");
	    LOGGER.info("**********************************************************************************");
	    try {
		driver = LanSideBasePage.getDriver();
		status = BroadBandWebUiUtils.isPreferredPrivateCheckBoxEnabled(driver,
			BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
	    } catch (Exception exception) {
		LOGGER.error("Exception occurred while clicking private wifi" + exception.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : prefer private option is in disabled state");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S13";
	    errorMessage = "Unable to retrieve the status of Prefer Private 'Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate' as false using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Verify the PreferPrivate option is disabled via WebPA");
	    LOGGER.info("STEP 13: ACTION : Execute the WEBPA Command To check the prefer private option is disabled.");
	    LOGGER.info(
		    "STEP 13: EXPECTED : Prefer Private Option Should be disabled and The WEBPA Command should return False.");
	    LOGGER.info("**********************************************************************************");
	    preferPrivateStatus = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS);
	    LOGGER.info("Prefer Private status retrieved using WebPa = " + preferPrivateStatus);
	    status = CommonMethods.isNotNull(preferPrivateStatus)
		    && preferPrivateStatus.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    errorMessage = errorMessage + ":Actual=" + preferPrivateStatus + " " + "Expected="
		    + BroadBandTestConstants.FALSE;
	    if (status) {
		LOGGER.info(
			"S13 ACTUAL: Successfully retrieved  the status of Prefer Private 'Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate' as false using WebPA command.");
	    } else {
		LOGGER.error("S13 ACTUAL: " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S14";
	    errorMessage = "The Filtering Mode for Public Wifi 2.4 (AccessPoint.10003) is not Allow-all after making prefer private option as disable";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION : Execute webPA Get command  on Device.WiFi.AccessPoint.10003.X_COMCAST-COM_MAC_FilteringMode ");
	    LOGGER.info(
		    "STEP 14: ACTION : Execute the command:curl -X GET -H \"Authorization: Bearer <SAT token>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i <WEBPA_SERVER_URL><CM_MAC>/config?names=Device.WiFi.AccessPoint.10003.X_COMCAST-COM_MAC_FilteringMode ");
	    LOGGER.info(
		    "STEP 14: EXPECTED : As the prefer private is false, the Filtering Mode for Public Wifi 2.4Ghz (AccessPoint.10003) should be Allow-All.");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("waiting for 1 mins to reflect");
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_COMCAST_COM_MAC_FILTERINGMODE);
	    LOGGER.info("value retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_ALLOW_ALL);
	    if (status) {
		LOGGER.info(
			"STEP 14: ACTUAL : As the prefer private is false, the Filtering Mode for Public Wifi 2.4Ghz (AccessPoint.10003) is Allow-All.");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S15";
	    errorMessage = "The Filtering Mode for Public Wifi 2.4 (AccessPoint.10103) is not Allow-all after making prefer Private option as disable";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION : Execute webPA Get command  on Device.WiFi.AccessPoint.10103.X_COMCAST-COM_MAC_FilteringMode  and  Device.WiFi.AccessPoint.10.X_COMCAST-COM_MAC_FilteringMode");
	    LOGGER.info(
		    "STEP 15: ACTION : Execute the command:curl -X GET -H \"Authorization: Bearer <SAT token>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i <WEBPA_SERVER_URL><CM_MAC>/config?names=Device.WiFi.AccessPoint.10103.X_COMCAST-COM_MAC_FilteringMode ");
	    LOGGER.info(
		    "STEP 15: EXPECTED : As the prefer private is true, the Filtering Mode for Public Wifi 5Ghz (AccessPoint.10103) should be Allow-All.");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_COMCAST_COM_MAC_FILTERINGMODE);
	    LOGGER.info("value retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_ALLOW_ALL);
	    if (status) {
		LOGGER.info(
			"STEP 15: ACTUAL : As the prefer private is false, the Filtering Mode for Public Wifi 5Ghz (AccessPoint.10103) is Allow-All.");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	} catch (Exception exception) {
	    LOGGER.error("Exception occurred while clicking private wifi" + exception.getMessage());
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("### POST-CONDITION : ### BEGIN SETTING THE PREFER PRIVATE MODE AS DEFAULT VALUE AS TRUE");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : BEGIN SETTING THE PREFER PRIVATE MODE AS DEFAULT VALUE AS TRUE.");
	    LOGGER.info("POST-CONDITION : ACTION : SETTING THE PREFER PRIVATE MODE AS DEFAULT VALUE AS TRUE ");
	    LOGGER.info("POST-CONDITION : EXPECTED : PREFER PRIVATE MODE SHOULD BE SET AS DEFAULT VALUE TRUE.");

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_PREFER_PRIVATE_CONNECTION,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    LOGGER.info("POST CONDITION :PREFER PRIVATE MODE AS DEFAULT VALUE AS TRUE " + status);
	    LOGGER.info("### POST-CONDITION ### END SETTING THE PREFER PRIVATE MODE AS DEFAULT VALUE AS TRUE.");
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    LanSideBasePage.closeBrowser();
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-PREFERPRIVATE-1002");
    }

    /**
     * Verify whether Firmware upgrade from old software to the software with these changes or Factory Resetting the
     * Device, should enable the Prefer Private functionality irrespective of the previous state.
     * <ol>
     * <li>Connect to the Connected client having 2.4GHZ wifi Capability.</li>
     * <li>Check if the wirless connected client has an IPv4 address from the gateway.</li>
     * <li>Verify whether you have connectivity in the connected client using IPV4 address.</li>
     * <li>Check if the wirless connected client has an IPv6 address from the gateway.</li>
     * <li>Verify whether you have connectivity in the connected client using IPV6 address.</li>
     * <li>Launch Broad band WebUI login page and verify login status</li>
     * <li>Launch the Connected Devices page from AdminUI page</li>
     * <li>Verify whether Prefer Private option is Checked(enabled) from Local GUI page</li>
     * <li>Verify the PreferPrivate option is enabled via WebPA</li>
     * <li>Perform Factory Reset of the device by executing the WEBPA Command.</li>
     * <li>Connect to the Connected client having 2.4GHZ wifi Capability.</li>
     * <li>Check if the wirless connected client has an IPv4 address from the gateway.</li>
     * <li>Verify whether you have connectivity in the connected client using IPV4 address.</li>
     * <li>Check if the wirless connected client has an IPv6 address from the gateway.</li>
     * <li>Verify whether you have connectivity in the connected client using IPV6 address.</li>
     * <li>Launch Broad band WebUI login page and verify login status</li>
     * <li>Launch the Connected Devices page from AdminUI page</li>
     * <li>Verify whether Prefer Private option is Checked(enabled) from Local GUI page</li>
     * <li>Verify the PreferPrivate option is enabled via WebPA</li>
     * 
     * </ol>
     * 
     * @author Joseph Maduram
     * @refactor Rakesh C N
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-WIFI-PREFERPRIVATE-1003")
    public void preferPrivateFunctionalityCheck(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI-PREFERPRIVATE-103";// Test case id
	String stepNum = "S1";// Test step number
	String errorMessage = null;// String to store the error message
	boolean status = false;// String to store the test case status
			       // firmware version.
	Dut connectedDeviceActivated = null;// Dut object to store client device
	boolean isFactoryReset = false;// stores the boolean value
	String preferPrivateStatus = "";// String variable to store the Prefer Private Status.
	WebDriver driver = null;

	// Variable Declation Ends

	testCaseId = "TC-RDKB-WIFI-PREFERPRIVATE-103";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-PREFERPRIVATE-1003");
	LOGGER.info(
		"TEST DESCRIPTION: Verify whether Firmware upgrade from old software to the software with these changes or Factory Resetting the "
			+ "Device, Should enable the Prefer Private functionality irrespective of the previous state.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Connect to the Connected client having  2.4GHZ wifi Capability.");
	LOGGER.info("2. Check if the wirless connected client has an IPv4 address from the gateway.");
	LOGGER.info("3. Verify whether you have connectivity in the connected client using IPV4 address.");
	LOGGER.info("4. Check if the wirless connected client has an IPv6 address from the gateway.");
	LOGGER.info("5. Verify whether you have connectivity in the connected client using IPV6 address.");
	LOGGER.info("6. Launch Broad band LAN login page and verify login status");
	LOGGER.info("7. Launch the Connected Devices page from AdminUI page");
	LOGGER.info("8. Verify whether Prefer Private option is Checked(enabled) from Local GUI page");
	LOGGER.info("9. Verify the PreferPrivate option is enabled via WebPA");
	LOGGER.info("10. Perform Factory Reset of the device by executing the WEBPA Command.");
	LOGGER.info("11. Connect to the Connected client having  2.4GHZ wifi Capability.");
	LOGGER.info("12. Check if the wirless connected client has an IPv4 address from the gateway.");
	LOGGER.info("13. Verify whether you have connectivity in the connected client using IPV4 address.");
	LOGGER.info("14. Check if the wirless connected client has an IPv6 address from the gateway.");
	LOGGER.info("15. Verify whether you have connectivity in the connected client using IPV6 address.");
	LOGGER.info("16. Launch Broad band LAN login page and verify login status");
	LOGGER.info("17. Launch the Connected Devices page from AdminUI page");
	LOGGER.info("18. Verify whether Prefer Private option is Checked(enabled) from Local GUI page");
	LOGGER.info("19. Verify the PreferPrivate option is enabled via WebPA");
	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "S1";
	    errorMessage = "Unable to connect to 2.4GHZ wifi";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Connect to the Connected client having  2.4GHZ wifi Capability");
	    LOGGER.info("STEP 1: ACTION : connect to the client having wifi capablilty as 2.4 ghz/Dual band");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Device should be able to connect with the connected client having 2.4GHZ wifi Capability");
	    LOGGER.info("**********************************************************************************");
	    connectedDeviceActivated = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    status = (null != connectedDeviceActivated);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Device is connected with the connected client having 2.4Ghz Wifi Capability");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    // verifying ipv4 and ipv6 and connectivity
	    BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, connectedDeviceActivated, 2);

	    stepNum = "S6";
	    errorMessage = "Unable to Login to LanGUI page using Admin credential";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Launch Broad band LAN UI login page and verify login status");
	    LOGGER.info(
		    "STEP 6: ACTION : Launch the below URL format in browserhttps://10.0.0.1/LOGIN CREDENTIALS :  username: 'admin' Password: 'password'");
	    LOGGER.info("STEP 6: EXPECTED : AdminUI page should be launched and login should be successful");
	    LOGGER.info("**********************************************************************************");
	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, connectedDeviceActivated);
	    driver = LanWebGuiLoginPage.getDriver();
	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Launch Broad band LAN UI login page and verify login status is successful");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S7";
	    errorMessage = "Connected Devices page is not  displayed having page title as \"Gateway > Connected Devices >Devices\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Launch the Connected Devices page from AdminUI page");
	    LOGGER.info(
		    "STEP 7: ACTION : Navigate to Connected Devices page by clicking on \"Connected Decvices\" Menu  and select the submenu \"Devices\" ");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Connected Devices page should get displayed having page title as \"Gateway > Connected Devices >Devices\"");
	    LOGGER.info("**********************************************************************************");
	    LanSidePageNavigation lanSidePageNavigation = new LanSidePageNavigation(driver);
	    try {
		
		status=lanSidePageNavigation.navigateToConnectedDevicesPage(device, tapEnv, driver);
	    } catch (Exception exception) {
		LOGGER.error("Exception occurred while navigating to connected devices page in LAN  GUI Page: "
			+ exception.getMessage());
	    }
	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : Connected Devices Page is displayed having Page Title as Gateway > Connected Devices >Devices");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S8";
	    errorMessage = "Prefer Private option is not Checked(Enabled) by default in Local GUI page.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify whether Prefer Private option is Checked(enabled) from Local GUI page");
	    LOGGER.info("STEP 8: ACTION : Verify the state of Prefer Private option from Local GUI page.");
	    LOGGER.info(
		    "STEP 8: EXPECTED : Prefer Private option should be Checked(Enabled) by default in Local GUI page.");
	    LOGGER.info("**********************************************************************************");
	    try {
		//driver = LanSideBasePage.getDriver();
		status = BroadBandWebUiUtils.isPreferredPrivateCheckBoxEnabled(driver,
			BroadBandTestConstants.BOOLEAN_VALUE_TRUE);
	    } catch (Exception exception) {
		LOGGER.error("Exception occurred while clicking private wifi" + exception.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : prefer private option is in enabled state");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S9";
	    errorMessage = "Unable to retrieve the status of Prefer Private 'Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate' using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify the PreferPrivate option is enabled via WebPA");
	    LOGGER.info("STEP 9: ACTION : Execute the WEBPA Command To check the prefer private option is enabled.");
	    LOGGER.info(
		    "STEP 9: EXPECTED : Prefer Private Option Should be Disabled and The WEBPA Command should return True.");
	    LOGGER.info("**********************************************************************************");
	    preferPrivateStatus = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS);
	    LOGGER.info("Prefer Private status retrieved using WebPa = " + preferPrivateStatus);
	    status = CommonMethods.isNotNull(preferPrivateStatus)
		    && preferPrivateStatus.equalsIgnoreCase(BroadBandTestConstants.TRUE);
	    errorMessage = errorMessage + ":Actual=" + preferPrivateStatus + " " + "Expected="
		    + BroadBandTestConstants.TRUE;
	    if (status) {
		LOGGER.info(
			"S9 ACTUAL: Successfully retrieved  the status of Prefer Private 'Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate' using WebPA command.");
	    } else {
		LOGGER.error("S9 ACTUAL: " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S10";
	    errorMessage = "Factory resetting the device failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Perform Factory Reset of the device by executing the WEBPA Command.");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute the command: curl -X SET -H \"Authorization: Bearer <SAT token>\" -X PATCH <WEBPA_SERVER_URL><CM_MAC>/config -d \'{\"parameters\":[{\"dataType\":3,\"name\":\"Device.X_CISCO_COM_DeviceControl.FactoryReset string \"Router,Wifi,VoIP,Dect,MoCA\"\",\"value\":\"true\"}]}'curl -X SET -H \"Authorization: Bearer <SAT token>\" -X PATCH <WEBPA_SERVER_URL><CM_MAC>/config -d \"{\"parameters\":[{\"dataType\":3,\"name\":\"Device.X_CISCO_COM_DeviceControl.FactoryReset string \"Router,Wifi,VoIP,Dect,MoCA\"\",\"value\":\"true\"}]}\"");
	    LOGGER.info("STEP 10: EXPECTED : The device should undergo Factory Reset.");
	    LOGGER.info("**********************************************************************************");
	    isFactoryReset = true;
	    status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);
	    isFactoryReset = status;
	    if (status) {
		errorMessage = "Reactivation of  the device failed after factory reset";
		BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
		LOGGER.info("STEP 10: ACTUAL : Factory Reset successful for the device and reactivated");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
	    LOGGER.info("waiting for 5 mins");

	    stepNum = "S11";
	    errorMessage = "Unable to connect to 2.4GHZ wifi";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Connect to the Connected client having  2.4GHZ wifi Capability");
	    LOGGER.info("STEP 11: ACTION : connect to the client having wifi capablilty as 2.4 ghz/Dual band");
	    LOGGER.info(
		    "STEP 11: EXPECTED : Device should be able to connect with the connected client having 2.4GHZ wifi Capability");
	    LOGGER.info("**********************************************************************************");

	    connectedDeviceActivated = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    status = (null != connectedDeviceActivated);
	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : Device is connected with the connected client having 2.4Ghz Wifi Capability");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    // verifying ipv4 and ipv6 and connectivity
	    BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, connectedDeviceActivated, 12);

	    stepNum = "S16";
	    errorMessage = "Unable to Login to LanGUI page using Admin credential";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Launch Broad band WebUI login page and verify login status");
	    LOGGER.info(
		    "STEP 16: ACTION : Launch the below URL format in browserhttps://10.0.0.1/LOGIN CREDENTIALS :  username: 'admin' Password: 'password'");
	    LOGGER.info("STEP 16: EXPECTED : AdminUI page should be launched and login should be successful");
	    LOGGER.info("**********************************************************************************");
	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, connectedDeviceActivated);
	    driver = LanWebGuiLoginPage.getDriver();
	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : Launch Broad band WebUI login page and login status is successful");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S17";
	    errorMessage = "Connected Devices page is not displayed having page title as \"Gateway > Connected Devices >Devices\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Launch the Connected Devices page from AdminUI page");
	    LOGGER.info(
		    "STEP 17: ACTION : Navigate to Connected Devices page by clicking on \"Connected Decvices\" Menu  and select the submenu \"Devices\" ");
	    LOGGER.info(
		    "STEP 17: EXPECTED : Connected Devices page should get displayed having page title as \"Gateway > Connected Devices >Devices\"");
	    LOGGER.info("**********************************************************************************");
	    try {
		status=lanSidePageNavigation.navigateToConnectedDevicesPage(device, tapEnv, driver);
	    } catch (Exception exception) {
		LOGGER.error("Exception occurred while navigating to connected devices page in LAN  GUI Page: "
			+ exception.getMessage());
	    }
	    if (status) {
		LOGGER.info(
			"STEP 17: ACTUAL : Connected Devices Page is displayed having Page Title as Gateway > Connected Devices >Devices");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S18";
	    errorMessage = "Prefer Private option is not  Checked(Enabled) by default in Local GUI page.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 18: DESCRIPTION : Verify whether Prefer Private option is Checked(enabled) from Local GUI page");
	    LOGGER.info("STEP 18: ACTION : Verify the state of Prefer Private option from Local GUI page.");
	    LOGGER.info(
		    "STEP 18: EXPECTED : Prefer Private option should be Checked(Enabled) by default in Local GUI page.");
	    LOGGER.info("**********************************************************************************");
	    try {
		//driver = LanSideBasePage.getDriver();
		status = BroadBandWebUiUtils.isPreferredPrivateCheckBoxEnabled(driver,
			BroadBandTestConstants.BOOLEAN_VALUE_TRUE);
	    } catch (Exception exception) {
		LOGGER.error("Exception occurred while clicking private wifi" + exception.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : prefer private option is in enabled state");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S19";
	    errorMessage = "Unable to retrieve the status of Prefer Private 'Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate' using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION : Verify the PreferPrivate option is enabled via WebPA");
	    LOGGER.info("STEP 19: ACTION : Execute the WEBPA Command To check the prefer private option is enabled");
	    LOGGER.info(
		    "STEP 19: EXPECTED : Prefer Private Option Should be Disabled and The WEBPA Command should return True.");
	    LOGGER.info("**********************************************************************************");
	    preferPrivateStatus = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS);
	    LOGGER.info("Prefer Private status retrieved using WebPa = " + preferPrivateStatus);
	    status = CommonMethods.isNotNull(preferPrivateStatus)
		    && preferPrivateStatus.equalsIgnoreCase(BroadBandTestConstants.TRUE);
	    errorMessage = errorMessage + ":Actual=" + preferPrivateStatus + " " + "Expected="
		    + BroadBandTestConstants.TRUE;
	    if (status) {
		LOGGER.info(
			"S19 ACTUAL: Successfully retrieved  the status of Prefer Private 'Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate' using WebPA command.");
	    } else {
		LOGGER.error("S19 ACTUAL: " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);

	} finally {
	    if (isFactoryReset) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info("### POST-CONDITION : ### BEGIN BROAD BAND DEVICE REACTIVATION.");
		LOGGER.info("POST-CONDITION : DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
		LOGGER.info("POST-CONDITION : ACTION : BROAD BAND DEVICE REACTIVATION. ");
		LOGGER.info("POST-CONDITION : EXPECTED : device should get reactivated");
		BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    LanSideBasePage.closeBrowser();
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-PREFERPRIVATE-1003");
    }

}
